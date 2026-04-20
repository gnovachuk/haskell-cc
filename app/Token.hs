{-# LANGUAGE GADTs #-}

module Token (Token (..), Keyword (..)) where

data Keyword
  = IntKw
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
  deriving (Show, Eq)
