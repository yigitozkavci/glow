module Codegen where

import Syntax
import Lexer
import Control.Monad.Identity
import Control.Monad.State hiding (State)
import qualified Data.Map.Strict as Map

type State s = StateT s Identity
type LookupMap = Map.Map String Int
type CodegenState = State Codegen Int

data Codegen = Codegen
  { varCount  :: Int
  , resultStr :: String
  , lookupTable :: LookupMap
  , varIndex :: Int
  } deriving Show

parseFunction :: CodegenState
parseFunction = state $ \s ->
  (0, s { varCount = varCount s + 1 })

initCodegen :: Codegen
initCodegen = Codegen
  { varCount = 0
  , resultStr = ""
  , lookupTable = Map.empty
  , varIndex = 0
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

genFunctionDecl :: Name -> Int -> CodegenState
genFunctionDecl name argCount = state $ \s ->
  (0, s { resultStr = resultStr s ++ "def " ++ name ++ "(" ++ argTypes ++ ") {\n" })
  where argTypes = sepWithCommas $ replicate argCount "i32"

genFunction :: Name -> [Expr] -> Expr -> CodegenState
genFunction name args ret = do
  genFunctionDecl name (length args)
  genFuncArgVars args
  -- genFunctionBody

genFuncArgVar :: String -> CodegenState
genFuncArgVar arg = state $ \s ->
  (0, s { lookupTable = Map.insert arg (varIndex s) (lookupTable s)
        , varIndex = varIndex s + 1
        }
  )

genFuncArgVars :: [Expr] -> CodegenState
genFuncArgVars [Var x] = genFuncArgVar x
genFuncArgVars ((Var x):xs) = genFuncArgVar x >> genFuncArgVars xs

sepWithCommas :: [String] -> String
sepWithCommas [x] = x
sepWithCommas (x:xs) = x ++ ", " ++ sepWithCommas xs

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
