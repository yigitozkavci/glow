module Codegen where

import Syntax
import Lexer
import Control.Monad.Identity
import Control.Monad.State hiding (State)

type State s = StateT s Identity

data Codegen = Codegen {
  varCount  :: Int,
  resultStr :: String
} deriving Show

type CodegenState = State Codegen Int

parseFunction :: CodegenState
parseFunction = state $ \s ->
  (0, s { varCount = varCount s + 1 })

initCodegen :: Codegen
initCodegen = Codegen
  { varCount = 0
  , resultStr = "init -> "
  }

{- Returns a state computation with given expression array -}
computeExpr :: [Expr] -> CodegenState
computeExpr [x] = genSingleExpr x
computeExpr (x:xs) = genSingleExpr x >> computeExpr xs

codegen :: [Expr] -> Codegen
codegen xs = execState (computeExpr xs) initCodegen

genSingleExpr :: Expr -> CodegenState
genSingleExpr expr =
  case expr of
    (Function name args ret) ->
      genFunction name args ret
    _ -> state $ \s -> (0, s { resultStr = resultStr s ++ "other!" })

genFunction :: Name -> [Expr] -> Expr -> CodegenState
genFunction _ _ _ = state $ \s ->
  (0, s { resultStr = resultStr s ++ "function!" })

insideFunction :: [Expr] -> Expr -> String
insideFunction args expr =
  ""

argSep :: [Expr] -> String
argSep [] = ""
argSep [last] = "i32"
argSep (x:xs) = "i32, " ++ argSep xs

showVar :: Expr -> String
showVar (Var name) = name
showVar _ = ""

{-
def wow(a, b, c) = a + b;

define void @wow(i32, i32, i32) {
  %3 = add i32 %0 %
}
 -}
