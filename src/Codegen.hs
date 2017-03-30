module Codegen where

import Syntax
import Lexer
import Control.Monad.Identity
import Control.Monad.State hiding (State)
import qualified Data.Map.Strict as Map

type State s = StateT s Identity
type LookupMap = Map.Map String Int
type CodegenState a = State Codegen a

data Codegen = Codegen
  { varCount  :: Int
  , resultStr :: String
  , lookupTable :: LookupMap
  , varIndex :: Int
  } deriving Show

parseFunction :: CodegenState ()
parseFunction = modify $ \s -> s { varCount = varCount s + 1 }

initCodegen :: Codegen
initCodegen = Codegen
  { varCount = 0
  , resultStr = ""
  , lookupTable = Map.empty
  , varIndex = 0
  }

{- Returns a state computation with given expression array -}
computeExpr :: [Expr] -> CodegenState ()
computeExpr [x] = genSingleExpr x
computeExpr (x:xs) = genSingleExpr x >> computeExpr xs

codegen :: [Expr] -> Codegen
codegen xs = execState (computeExpr xs) initCodegen

-- varToStrings :: [Expr] -> [String]
-- varToStrings (Var x:xs) = x : varToStrings xs
-- varToStrings _ = error "Non-variable used as function argument"

genSingleExpr :: Expr -> CodegenState ()
genSingleExpr expr =
  case expr of
    (Function name args ret) ->
      genFunction name args ret
    -- (BinOp op left right) ->
      -- leftVal <- ensureVar left
      -- rightVal <- ensureVar right
      -- case op of
      --   Plus -> genPlusExpr left right
      --   Minus -> genMinusExpr left right
      --   Times -> genTimesExpr left right
      --   Divide -> genDivideExpr left right
    _ -> emptyState' expr

-- ensureFloat :: Expr -> CodegenState
-- ensureFloat expr =
--   case expr of
--     (Float num) -> return num
--     (Var a) -> state $ \s ->

genOpExpr :: Expr -> Expr -> CodegenState ()
genOpExpr (Float a) (Float b) = emptyState
-- genPlusExpr :: Expr -> Expr -> CodegenState
-- genPlusExpr (Var a) (Var b) = state $ \s ->
--   (0.0, s { resultStr = resultStr s ++ "%" ++ show (varIndex s) ++ " = add i32 %" ++ a ++ ", %" ++ b ++ "\n" })

-- genMinusExpr :: Expr -> Expr -> CodegenState
-- genMinusExpr (Var a) (Var b) = emptyState

-- genTimesExpr :: Expr -> Expr -> CodegenState
-- genTimesExpr (Var a) (Var b) = emptyState

-- genDivideExpr :: Expr -> Expr -> CodegenState
-- genDivideExpr (Var a) (Var b) = emptyState

-- This is for 'unimplemented' causes only. Should not be shipped into the production
emptyState :: CodegenState ()
emptyState = modify $ \s -> s { resultStr = resultStr s ++ "[!] non-implemented expression!\n" }

emptyState' :: Expr -> CodegenState ()
emptyState' expr = modify $ \s -> s { resultStr = resultStr s ++ "[!] non-implemented expression:\n[!] " ++ show expr ++ "\n" }

genFunctionDecl :: Name -> Int -> CodegenState ()
genFunctionDecl name argCount = modify $ \s ->
  s { resultStr = resultStr s ++ "def " ++ name ++ "(" ++ argTypes ++ ") {\n" }
  where argTypes = sepWithCommas $ replicate argCount "i32"

genFunction :: Name -> [String] -> Expr -> CodegenState ()
genFunction name args ret = do
  genFunctionDecl name (length args)
  genFuncArgVars args
  genSingleExpr ret
  genFuncEnd

genFuncArgVar :: String -> CodegenState ()
genFuncArgVar arg = modify $ \s ->
  s { lookupTable = Map.insert arg (varIndex s) (lookupTable s)
    , varIndex = varIndex s + 1 }

genFuncArgVars :: [String] -> CodegenState ()
genFuncArgVars [x] = genFuncArgVar x
genFuncArgVars (x:xs) = genFuncArgVar x >> genFuncArgVars xs

genFuncEnd :: CodegenState ()
genFuncEnd = modify $ \s ->
  s { resultStr = resultStr s ++ "}\n\n" }

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
define void @wow(i32, i32, i32) {
  %3 = add i32 %0 %
}

def wow(a, b, c) = a + b;
 -}
