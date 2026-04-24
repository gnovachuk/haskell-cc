{-# LANGUAGE LambdaCase #-}

{- HLINT ignore "Use <$>" -}

module Parser (Parser (..), parseProgram, Result (..)) where

import AST
import Token

data Result a
  = Ok a [Located Token]
  | SoftErr String (Maybe SourcePos) -- parser failed, no tokens consumed
  | HardErr String (Maybe SourcePos) -- parser failed, tokens were consumed

newtype Parser a = Parser {runParser :: [Located Token] -> Result a}

instance Functor Result where
  fmap f (Ok a rest) = Ok (f a) rest
  fmap _ (SoftErr msg pos) = SoftErr msg pos
  fmap _ (HardErr msg pos) = HardErr msg pos

instance Functor Parser where
  fmap f p =
    Parser (fmap f . runParser p)

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure a = Parser (Ok a)
  pf <*> pa =
    Parser
      ( \tokens -> do
          case runParser pf tokens of
            SoftErr msg pos -> SoftErr msg pos
            HardErr msg pos -> HardErr msg pos
            Ok f rest ->
              case runParser pa rest of
                Ok a rest' -> Ok (f a) rest'
                SoftErr msg pos | rest /= tokens -> HardErr msg pos -- turn into hard error if tokens were consumed
                HardErr msg pos -> HardErr msg pos
                SoftErr msg pos -> SoftErr msg pos
      )

instance Monad Parser where
  p >>= f =
    Parser
      ( \tokens ->
          case runParser p tokens of
            HardErr msg pos -> HardErr msg pos
            SoftErr msg pos -> SoftErr msg pos
            Ok a rest ->
              case runParser (f a) rest of
                SoftErr msg pos | rest /= tokens -> HardErr msg pos -- turn into hard error if tokens were consumed
                result -> result
      )

many :: Parser a -> Parser [a]
many p =
  ( do
      x <- p
      rest <- many p
      pure (x : rest)
  )
    `orElse` pure []

expect :: Token -> Parser ()
expect t =
  Parser
    ( \case
        [] -> SoftErr ("expected token " ++ show t) Nothing
        (t' : rest) ->
          if unLoc t' == t
            then Ok () rest
            else SoftErr ("expected token " ++ show t) (Just (locPos t'))
    )

satisfy :: (Token -> Maybe a) -> Parser a
satisfy f =
  Parser
    ( \case
        [] -> SoftErr "Unexpected end of input" Nothing
        (t : rest) -> case f (unLoc t) of
          Just a -> Ok a rest
          Nothing -> SoftErr ("Unexpected token " ++ show (unLoc t)) (Just (locPos t))
    )

(<?>) :: Parser a -> String -> Parser a -- relabel SoftErr
p <?> msg =
  Parser
    ( \tokens -> case runParser p tokens of
        SoftErr _ pos -> SoftErr msg pos
        other -> other
    )

infixl 3 `orElse`

infix 0 <?>

orElse :: Parser a -> Parser a -> Parser a
orElse p1 p2 =
  Parser
    ( \tokens ->
        case runParser p1 tokens of
          Ok a rest -> Ok a rest
          SoftErr _ _ -> runParser p2 tokens -- didn't consume, try other parser, p2.
          HardErr msg pos -> HardErr msg pos -- consumed, give up.
    )

parseLiteral :: Parser Expr
parseLiteral = LitExpr <$> satisfy (\case Literal n -> Just n; _ -> Nothing)

chainL1 :: Parser Expr -> Parser Op -> Parser Expr
chainL1 pOperand pOp = do
  lhs <- pOperand
  rest lhs
  where
    rest lhs =
      ( do
          op <- pOp
          rhs <- pOperand
          rest (BinOp op lhs rhs) -- recursive call with accumulated left operand.
      )
        `orElse` pure lhs

parseExpr :: Parser Expr
parseExpr = parseComma

parseComma :: Parser Expr
parseComma = chainL1 parseAssign (do expect Comma; pure CommaOp)

parseAssign :: Parser Expr
parseAssign = do
  lhs <- parseTernary
  ( do
      expect Equal
      rhs <- parseAssign -- right associative
      pure (Assign lhs rhs)
    )
    `orElse` pure lhs

parseTernary :: Parser Expr
parseTernary = do
  cond <- parseLogicalOr
  ( do
      expect Question
      thenExpr <- parseExpr
      expect Colon
      elseExpr <- parseTernary
      pure (TernaryOp cond thenExpr elseExpr)
    )
    `orElse` pure cond

parseLogicalOr :: Parser Expr
parseLogicalOr = do
  lhs <- parseLogicalAnd
  rest lhs
  where
    rest lhs =
      ( do
          expect Or
          rhs <- parseLogicalAnd
          rest (LogicalOr lhs rhs) -- recursive call with accumulated left operand.
      )
        `orElse` pure lhs

parseLogicalAnd :: Parser Expr
parseLogicalAnd = do
  lhs <- parseEquality
  rest lhs
  where
    rest lhs =
      ( do
          expect And
          rhs <- parseEquality
          rest (LogicalAnd lhs rhs) -- recursive call with accumulated left operand.
      )
        `orElse` pure lhs

parseEquality :: Parser Expr
parseEquality = chainL1 parseComparison parseEqualityOp

parseComparison :: Parser Expr
parseComparison = chainL1 parseTerm parseComparisonOp

parseTerm :: Parser Expr
parseTerm = chainL1 parseFactor parseAddOp

parseFactor :: Parser Expr
parseFactor = chainL1 parseCall parseMulOp

parseCall :: Parser Expr
parseCall = do
  callable <- parsePrimary
  ( do
      expect OpenParen
      args <-
        ( do
            first <- parseAssign -- not parseExpr because it contains Comma operator (we don't want to parse here)
            rest <-
              many
                ( do
                    expect Comma
                    parseExpr
                )
            pure (first : rest)
          )
          `orElse` pure []
      expect CloseParen
      pure (Call callable args)
    )
    `orElse` pure callable

parsePrimary :: Parser Expr
parsePrimary = parseLiteral `orElse` parseVarExpr

parseVarExpr :: Parser Expr
parseVarExpr = VarExpr <$> satisfy (\case Identifier ident -> Just ident; _ -> Nothing)

parseEqualityOp :: Parser Op
parseEqualityOp =
  satisfy
    ( \case
        EqualEqual -> Just EqualOp
        NotEqual -> Just NotEqualOp
        _ -> Nothing
    )

parseComparisonOp :: Parser Op
parseComparisonOp =
  satisfy
    ( \case
        Greater -> Just GreaterOp
        Less -> Just LessOp
        GreaterEqual -> Just GreaterEqualOp
        LessEqual -> Just LessEqualOp
        _ -> Nothing
    )

parseAddOp :: Parser Op
parseAddOp =
  satisfy
    ( \case
        Plus -> Just Add
        Minus -> Just Sub
        _ -> Nothing
    )

parseMulOp :: Parser Op
parseMulOp =
  satisfy
    ( \case
        Multiply -> Just Mul
        _ -> Nothing
    )

parseStmt :: Parser Stmt
parseStmt =
  parseVarDecl
    `orElse` parseReturn
    `orElse` parseBreak
    `orElse` parseContinue
    `orElse` parsePrint
    `orElse` parseBlock
    `orElse` parseIf
    `orElse` parseWhile
    `orElse` parseExprStmt

parseBreak :: Parser Stmt
parseBreak = do
  expect $ Keyword BreakKw
  expect Semicolon
  pure Break

parseContinue :: Parser Stmt
parseContinue = do
  expect $ Keyword ContinueKw
  expect Semicolon
  pure Continue

parseReturn :: Parser Stmt
parseReturn = do
  expect $ Keyword ReturnKw
  expr <- parseExpr
  expect Semicolon
  pure (Return expr)

parseExprStmt :: Parser Stmt
parseExprStmt = do
  expr <- parseExpr
  expect Semicolon
  pure (ExprStmt expr)

parseVarDecl :: Parser Stmt
parseVarDecl = do
  expect $ Keyword IntKw
  ident <- satisfy (\case Identifier ident -> Just ident; _ -> Nothing)
  expect Equal
  expr <- parseExpr
  expect Semicolon
  pure (VarDecl ident expr)

parsePrint :: Parser Stmt -- temporary solution to printing
parsePrint = do
  expect $ Keyword PrintKw
  expr <- parseExpr
  expect Semicolon
  pure (Print expr)

parseBlock :: Parser Stmt
parseBlock = do
  expect OpenBrace
  stmts <- many parseStmt
  expect CloseBrace
  pure (Block stmts)

parseIf :: Parser Stmt
parseIf = do
  expect $ Keyword IfKw
  expect OpenParen <?> "expected `(` after if"
  cond <- parseExpr
  expect CloseParen
  body <- parseStmt
  elseBranch <- parseElse `orElse` pure Nothing
  pure (If cond body elseBranch)

parseElse :: Parser (Maybe Stmt)
parseElse = do
  expect $ Keyword ElseKw
  stmt <- parseStmt
  pure (Just stmt)

parseWhile :: Parser Stmt
parseWhile = do
  expect $ Keyword WhileKw
  expect OpenParen
  cond <- parseExpr
  expect CloseParen
  body <- parseStmt
  pure (While cond body)

parseProgram :: Parser [Decl]
parseProgram = do
  decls <- many parseDecl
  parseEof
  pure decls

parseEof :: Parser ()
parseEof =
  Parser
    ( \tokens ->
        case tokens of
          [] -> Ok () []
          (t : _) -> HardErr ("Unexpected tokens after program: " ++ show (unLoc t)) (Just (locPos t))
    )

parseDecl :: Parser Decl
parseDecl = parseFuncDecl

parseFuncDecl :: Parser Decl
parseFuncDecl = do
  expect $ Keyword IntKw
  funcIdent <- satisfy (\case Identifier id -> Just id; _ -> Nothing)
  expect OpenParen

  params <-
    ( do
        first <- parseParam
        rest <- many parseCommaParam
        pure (first : rest)
      )
      `orElse` pure []

  expect CloseParen
  body <- parseBlock
  pure $ FuncDecl funcIdent params body

parseParam :: Parser String
parseParam = do
  expect $ Keyword IntKw
  satisfy (\case Identifier id -> Just id; _ -> Nothing)

parseCommaParam :: Parser String
parseCommaParam = do
  expect Comma
  parseParam
