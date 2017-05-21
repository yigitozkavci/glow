module Syntax where
import Data.Word (Word32)
import qualified LLVM.General.AST as AST
import Codegen

type Name = String

data PrimType = DoublePrim
              | CharPrim
              deriving (Eq, Ord, Show)

data TypeDecl = DoubleDecl
              | ArrayDecl PrimType
              deriving (Eq, Ord, Show)

data TypedName = TypedName TypeDecl Name deriving (Eq, Ord, Show)

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

declToType :: TypeDecl -> AST.Type
declToType DoubleDecl = double
declToType (ArrayDecl DoublePrim) = ptr double
declToType (ArrayDecl CharPrim) = ptr char
