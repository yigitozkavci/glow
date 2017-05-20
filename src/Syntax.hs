module Syntax where
import Data.Word (Word32)

type Name = String

data TypeDecl = DoubleDecl
              | ArrayDecl

data Typed a = Typed String a deriving (Eq, Ord, Show)

type TypedName = Typed Name

data Expr
  = Float Double
  | Array [Double]
  | ArrAccess Name Word32
  | Var String
  | Call Name [Expr]
  | Function Name [TypedName] Expr
  | Extern Name [Name]
  | BinaryOp Name Expr Expr
  | UnaryOp Name Expr
  | If Expr Expr Expr
  | For Name Expr Expr Expr Expr
  | BinaryDef Name [TypedName] Expr
  | UnaryDef Name TypedName Expr
  | Let Name Expr Expr
  deriving (Eq, Ord, Show)
