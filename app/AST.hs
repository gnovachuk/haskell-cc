module AST (Expr (..), Op (..)) where

data Op
  = Add
  | Mul
  deriving (Show)

data Expr
  = LitExpr Int
  | BinOp Op Expr Expr
  deriving (Show)
