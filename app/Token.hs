{-# LANGUAGE GADTs #-}

module Token (Token (..), Keyword (..)) where

data Keyword
  = IntKw
  | PrintKw -- Temporary Solution
  | IfKw
  | ElseKw
  | WhileKw
  | BreakKw
  | ContinueKw
  deriving (Show, Eq)

data Token where
  Literal :: Int -> Token
  Keyword :: Keyword -> Token
  Identifier :: String -> Token
  -- Operators
  Plus :: Token
  Minus :: Token
  Multiply :: Token
  Equal :: Token
  Semicolon :: Token
  OpenParen :: Token
  CloseParen :: Token
  OpenBrace :: Token
  CloseBrace :: Token
  deriving (Show, Eq)
