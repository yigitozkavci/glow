module Syntax where

type Name = String

data Expr
  = Float Double
  | Var String
  | Call Name [Expr]
  | Function Name [Name] Expr
  | Extern Name [Name]
  | BinaryOp Name Expr Expr
  | UnaryOp Name Expr
  | If Expr Expr Expr
  | For Name Expr Expr Expr Expr
  deriving (Eq, Ord, Show)
