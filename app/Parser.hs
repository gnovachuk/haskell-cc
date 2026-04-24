{-# LANGUAGE LambdaCase #-}

{- HLINT ignore "Use <$>" -}

module Parser (Parser (..), parseProgram, Result (..)) where

import AST
import Token

data Result a
  = Ok a [Token]
  | SoftErr String -- parser failed, no tokens consumed
  | HardErr String -- parser failed, tokens were consumed

newtype Parser a = Parser {runParser :: [Token] -> Result a}

instance Functor Result where
  fmap f (Ok a rest) = Ok (f a) rest
  fmap _ (SoftErr msg) = SoftErr msg
  fmap _ (HardErr msg) = HardErr msg

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
            SoftErr msg -> SoftErr msg
            HardErr msg -> HardErr msg
            Ok f rest ->
              case runParser pa rest of
                Ok a rest' -> Ok (f a) rest'
                SoftErr msg | rest /= tokens -> HardErr msg -- turn into hard error if tokens were consumed
                HardErr msg -> HardErr msg
                SoftErr msg -> SoftErr msg
      )

instance Monad Parser where
  p >>= f =
    Parser
      ( \tokens ->
          case runParser p tokens of
            HardErr msg -> HardErr msg
            SoftErr msg -> SoftErr msg
            Ok a rest ->
              case runParser (f a) rest of
                SoftErr msg | rest /= tokens -> HardErr msg -- turn into hard error if tokens were consumed
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
        [] -> SoftErr ("expected token " ++ show t)
        (t' : rest) ->
          if t' == t
            then Ok () rest
            else SoftErr ("expected token " ++ show t)
    )

satisfy :: (Token -> Maybe a) -> Parser a
satisfy f =
  Parser
    ( \case
        [] -> SoftErr "Unexpected end of input"
        (t : rest) -> case f t of
          Just a -> Ok a rest
          Nothing -> SoftErr ("Unexpected token" ++ show t)
    )

(<?>) :: Parser a -> String -> Parser a -- relabel SoftErr
p <?> msg =
  Parser
    ( \tokens -> case runParser p tokens of
        SoftErr _ -> SoftErr msg
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
          SoftErr _ -> runParser p2 tokens -- didn't consume, try other parser, p2.
          HardErr msg -> HardErr msg -- consumed, give up.
    )

parseLiteral :: Parser Expr
parseLiteral = LitExpr <$> satisfy (\case Literal n -> Just n; _ -> Nothing)

parseExpr :: Parser Expr
parseExpr = parseAssign

parseAssign :: Parser Expr
parseAssign = do
  lhs <- parseTerm
  ( do
      expect Equal
      rhs <- parseAssign -- right associative
      pure (Assign lhs rhs)
    )
    `orElse` pure lhs

parseTerm :: Parser Expr
parseTerm = do
  lhs <- parseFactor
  ( do
      op <- parseAddOp
      rhs <- parseTerm
      pure (BinOp op lhs rhs)
    )
    `orElse` pure lhs

parseFactor :: Parser Expr
parseFactor = do
  lhs <- parseCall
  ( do
      op <- parseMulOp
      rhs <- parseFactor
      pure (BinOp op lhs rhs)
    )
    `orElse` pure lhs

parseCall :: Parser Expr
parseCall = do
  callable <- parsePrimary
  ( do
      expect OpenParen
      args <-
        ( do
            first <- parseExpr
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
          (t : _) -> HardErr ("Unexpected tokens after program: " ++ show t)
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
