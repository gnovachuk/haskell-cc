module AST (Expr (..), Stmt (..), Decl (..), Op (..)) where

data Op
  = Add
  | Sub
  | Mul
  deriving (Show)

data Expr
  = LitExpr Int
  | VarExpr String
  | BinOp Op Expr Expr
  | Assign Expr Expr
  | Call Expr [Expr]
  deriving (Show)

data Stmt
  = Break
  | Continue
  | VarDecl String Expr
  | ExprStmt Expr
  | Print Expr
  | Block [Stmt]
  | If Expr Stmt (Maybe Stmt)
  | While Expr Stmt
  deriving (Show)

data Decl
  = FuncDecl String [String] Stmt
