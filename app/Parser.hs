{-# LANGUAGE LambdaCase #-}

{- HLINT ignore "Use <$>" -}

module Parser (Parser (..), parseStmt) where

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
parseExpr = do
  lhs <- parseTerm
  ( do
      op <- parseAddOp
      rhs <- parseExpr
      pure (BinOp op lhs rhs)
    )
    `orElse` pure lhs

parseTerm :: Parser Expr
parseTerm = do
  lhs <- parseFactor
  ( do
      op <- parseMulOp
      rhs <- parseTerm
      pure (BinOp op lhs rhs)
    )
    `orElse` pure lhs

parseFactor :: Parser Expr
parseFactor = parseLiteral `orElse` parseVarExpr

parseVarExpr :: Parser Expr
parseVarExpr = VarExpr <$> satisfy (\case Identifier ident -> Just ident; _ -> Nothing)

parseAddOp :: Parser Op
parseAddOp =
  satisfy
    ( \case
        Plus -> Just Add
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
parseStmt = do
  stmt <-
    parseVarDecl
      `orElse` parsePrint
      `orElse` parseBlock
  expect Semicolon
  pure stmt

parseVarDecl :: Parser Stmt
parseVarDecl = do
  expect $ Keyword IntKw
  id <- satisfy (\case Identifier id -> Just id; _ -> Nothing)
  expect Equal
  expr <- parseExpr
  pure (VarDecl id expr)

parsePrint :: Parser Stmt -- temporary solution to printing
parsePrint = do
  expect $ Keyword PrintKw
  expr <- parseExpr
  pure (Print expr)

parseBlock :: Parser Stmt
parseBlock = do
  expect OpenBrace
  stmts <- many parseStmt
  expect CloseBrace
  pure (Block stmts)
