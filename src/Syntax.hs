module Syntax where

type Name = String

data Expr = Float Double
          | BinOp Op Expr Expr
          | Var String
          | Call Name [Expr]
          | Function Name [Expr] Expr
          | Extern Name [Expr]
          deriving (Show, Ord, Eq)

data Op = Plus
        | Minus
        | Times
        | Divide
        deriving (Show, Ord, Eq)
