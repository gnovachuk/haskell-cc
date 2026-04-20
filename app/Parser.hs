{-# LANGUAGE LambdaCase #-}

{- HLINT ignore "Use <$>" -}

module Parser (Parser (..), parseExpr) where

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

tokenToOp :: Token -> Maybe Op
tokenToOp Plus = Just Add
tokenToOp Multiply = Just Mul
tokenToOp _ = Nothing

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
parseFactor = parseLiteral

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
