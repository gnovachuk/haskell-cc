module AST (Expr (..), Stmt (..), Op (..)) where

data Op
  = Add
  | Mul
  deriving (Show)

data Expr
  = LitExpr Int
  | VarExpr String
  | BinOp Op Expr Expr
  deriving (Show)

data Stmt
  = Assign String Expr
  | VarDecl String Expr
  | Print Expr
  | Block [Stmt]
  | If Expr Stmt (Maybe Stmt)
  deriving (Show)
