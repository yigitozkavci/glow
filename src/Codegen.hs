module Codegen where

import Syntax
import Lexer
import Control.Monad.Identity
import Control.Monad.State hiding (State)
import qualified Data.Map.Strict as Map
import Data.Maybe

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
    (BinOp op left right) -> do
      leftVal <- ensureVar left -- It might not be a Var though.
      rightVal <- ensureVar right
      case op of
        Plus -> genPlusExpr leftVal rightVal
        Minus -> genMinusExpr leftVal rightVal
        Times -> genTimesExpr leftVal rightVal
        Divide -> genDivideExpr leftVal rightVal
    _ -> emptyState' expr

ensureVar :: Expr -> CodegenState Int
ensureVar expr =
  case expr of
    (Var a) -> state $ \s ->
      let var = Map.lookup a (lookupTable s) in
      case var of
        Just a -> (a, s)
        Nothing -> error $ "Variable " ++ a ++ " is not defined!"
    (Float num) -> state $ \s ->
      (varIndex s, s { resultStr = resultStr s ++ defFloat (varIndex s) num
                     , varIndex = varIndex s + 1
                     })

defFloat :: Int -> Double -> String
defFloat a b = "%" ++ show a ++ " = add i32 0, " ++ show b ++ "\n"

genOpExpr :: Expr -> Expr -> CodegenState ()
genOpExpr (Float a) (Float b) = emptyState

genPlusExpr :: Int -> Int -> CodegenState ()
genPlusExpr a b = modify $ \s ->
  s { resultStr = resultStr s ++ "%" ++ show (varIndex s) ++ " = add i32 %" ++ show a ++ ", %" ++ show b ++ "\n"
    , varIndex = varIndex s + 1 
    }

genMinusExpr :: Int -> Int -> CodegenState ()
genMinusExpr a b = modify $ \s ->
  s { resultStr = resultStr s ++ "%" ++ show (varIndex s) ++ " = add i32 %" ++ show a ++ ", %" ++ show b ++ "\n"
    , varIndex = varIndex s + 1 
    }

genTimesExpr :: Int -> Int -> CodegenState ()
genTimesExpr a b = modify $ \s ->
  s { resultStr = resultStr s ++ "%" ++ show (varIndex s) ++ " = add i32 %" ++ show a ++ ", %" ++ show b ++ "\n"
    , varIndex = varIndex s + 1 
    }

genDivideExpr :: Int -> Int -> CodegenState ()
genDivideExpr a b = modify $ \s ->
  s { resultStr = resultStr s ++ "%" ++ show (varIndex s) ++ " = add i32 %" ++ show a ++ ", %" ++ show b ++ "\n"
    , varIndex = varIndex s + 1 
    }

-- This is for 'unimplemented' causes only. Should not be shipped into the production
emptyState :: CodegenState ()
emptyState = modify $ \s -> s { resultStr = resultStr s ++ "[!] non-implemented expression!\n" }

emptyState' :: Expr -> CodegenState ()
emptyState' expr = modify $ \s -> s { resultStr = resultStr s ++ "[!] non-implemented expression:\n[!] " ++ show expr ++ "\n" }

genFunction :: Name -> [Expr] -> Expr -> CodegenState ()
genFunction name args ret = do
  genFunctionDecl name (length args)
  genFuncArgVars args
  genFuncLabelVar
  genSingleExpr ret
  genFuncEnd

genFunctionDecl :: Name -> Int -> CodegenState ()
genFunctionDecl name argCount = modify $ \s ->
  s { resultStr = resultStr s ++ "define i32 @" ++ name ++ "(" ++ argTypes ++ ") {\n" }
  where argTypes = sepWithCommas $ replicate argCount "i32"

genFuncLabelVar :: CodegenState ()
genFuncLabelVar = modify $ \s ->
  s { varIndex = varIndex s + 1 } -- Should we record label var?

genFuncArgVar :: Expr -> CodegenState ()
genFuncArgVar (Var arg) = modify $ \s ->
  s { lookupTable = Map.insert arg (varIndex s) (lookupTable s)
    , varIndex = varIndex s + 1 }

genFuncArgVars :: [Expr] -> CodegenState ()
genFuncArgVars [x] = genFuncArgVar x
genFuncArgVars (x:xs) = genFuncArgVar x >> genFuncArgVars xs

genFuncEnd :: CodegenState ()
genFuncEnd = modify $ \s ->
  s { resultStr = resultStr s ++ "ret i32 %" ++ show (varIndex s - 1) ++ "\n}\n\n"
    , varIndex = 0
    }

sepWithCommas :: [String] -> String
sepWithCommas [x] = x
sepWithCommas (x:xs) = x ++ ", " ++ sepWithCommas xs

insideFunction :: [Expr] -> Expr -> String
insideFunction args expr =
  ""

argSep :: [Expr] -> String
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
