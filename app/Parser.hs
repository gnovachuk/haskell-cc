{-# LANGUAGE LambdaCase #-}

{- HLINT ignore "Use <$>" -}

module Parser (Parser (..), parseProgram) where

import AST
import Token

newtype Parser a = Parser {runParser :: [Token] -> Either String (a, [Token])}

instance Functor Parser where
  fmap f p =
    Parser (\tokens -> fmap (\(a, rest) -> (f a, rest)) (runParser p tokens))

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure a = Parser (\tokens -> Right (a, tokens))
  pf <*> pa =
    Parser
      ( \tokens -> do
          (f, rest) <- runParser pf tokens
          (a, rest') <- runParser pa rest
          Right (f a, rest')
      )

instance Monad Parser where
  p >>= f =
    Parser
      ( \tokens ->
          case runParser p tokens of
            Left msg -> Left msg
            Right (a, rest) -> runParser (f a) rest
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
        [] -> Left ("expected token " ++ show t)
        (t' : rest) ->
          if t' == t
            then Right ((), rest)
            else Left ("expected token " ++ show t)
    )

satisfy :: (Token -> Maybe a) -> Parser a
satisfy f =
  Parser
    ( \case
        [] -> Left "Unexpected end of input"
        (t : rest) -> case f t of
          Just a -> Right (a, rest)
          Nothing -> Left ("Unexpected token" ++ show t)
    )

anyToken :: Parser Token
anyToken = satisfy Just

orElse :: Parser a -> Parser a -> Parser a
orElse p1 p2 =
  Parser
    ( \tokens ->
        case runParser p1 tokens of
          Left _ -> runParser p2 tokens
          right -> right
    )

failParser :: String -> Parser a
failParser msg = Parser (\_ -> Left msg)

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
  expect OpenParen
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
parseProgram = many parseDecl

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
