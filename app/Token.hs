{-# LANGUAGE GADTs #-}

module Token (Token (..), Keyword (..)) where

data Keyword
  = IntKw
  | PrintKw -- Temporary Solution
  deriving (Show, Eq)

data Token where
  Literal :: Int -> Token
  Keyword :: Keyword -> Token
  Identifier :: String -> Token
  -- Operators
  Plus :: Token
  Multiply :: Token
  Equal :: Token
  Semicolon :: Token
  OpenParen :: Token
  CloseParen :: Token
  OpenBrace :: Token
  CloseBrace :: Token
  deriving (Show, Eq)
