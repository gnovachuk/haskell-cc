{-# LANGUAGE GADTs #-}

module Token (Located (..), SourcePos (..), Token (..), Keyword (..)) where

data SourcePos = SourcePos
  { line :: Int,
    col :: Int
  }
  deriving (Show, Eq)

data Located a = Located
  { unLoc :: a,
    locPos :: SourcePos
  }
  deriving (Show, Eq)

data Keyword
  = IntKw
  | PrintKw -- Temporary Solution
  | IfKw
  | ElseKw
  | WhileKw
  | BreakKw
  | ContinueKw
  | VoidKw
  | ReturnKw
  deriving (Show, Eq)

data Token where
  Literal :: Int -> Token
  Keyword :: Keyword -> Token
  Identifier :: String -> Token
  -- Operators
  Comma :: Token
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
