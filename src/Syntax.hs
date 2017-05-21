module Syntax where

import Data.Word (Word32)
import qualified LLVM.General.AST as AST
import Codegen

type Name = String

data PrimType = DoublePrim
              | IntPrim
              deriving (Eq, Ord, Show)

data TypeDecl = DoubleDecl
              | IntDecl
              | CharDecl
              | ArrayDecl PrimType
              deriving (Eq, Ord, Show)

data TypedName = TypedName TypeDecl Name deriving (Eq, Ord, Show)

data Expr
  = Float Double
  | Integer Integer
  | Char Char
  | IntArray [Integer]
  | DoubleArray [Double]
  | ArrAccess Name Word32
  | Var String
  | Call Name [Expr]
  | Function TypeDecl Name [TypedName] Expr
  | Extern TypeDecl Name [TypedName]
  | BinaryOp Name Expr Expr
  | UnaryOp Name Expr
  | If Expr Expr Expr
  | For Name Expr Expr Expr Expr
  | BinaryDef TypeDecl Name [TypedName] Expr
  | UnaryDef TypeDecl Name TypedName Expr
  | Let Name Expr Expr
  deriving (Eq, Ord, Show)

declToType :: TypeDecl -> AST.Type
declToType DoubleDecl = double
declToType IntDecl = int
declToType CharDecl = int
declToType (ArrayDecl DoublePrim) = ptr double
declToType (ArrayDecl IntPrim) = ptr int
