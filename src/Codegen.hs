module Codegen where

import Syntax
import Lexer
import Control.Monad.Identity
import Control.Monad.State hiding (State)
import qualified Data.Map.Strict as Map
import Data.Maybe

type State s = StateT s Identity
type LookupTable = Map.Map String Int
type CodegenState a = State Codegen a
type Block = String

data Scope = Global | Local
data Codegen = Codegen
  { varCount  :: Int
  , resultStr :: String
  , globalLookupTable :: LookupTable
  , localLookupTable :: LookupTable
  , currentScope :: Scope
  , varIndex :: Int
  , globalBlock :: Block
  , funcBlocks :: [Block]
  , mainBlock :: Block
  , currentBlock :: Block
  , globalVars :: [String]
  }

{- Block structure:
 - |================================== |
 - | Global Variable Decleration Block |
 - |================================== |
 - | Function Decleration Blocks       | TODO: Take care of function orders. In LLVM, that matters.
 - |================================== |
 - | Main Method Block                 |
 - |================================== |
 -}
emptyLookupTable :: LookupTable
emptyLookupTable = Map.empty

parseFunction :: CodegenState ()
parseFunction = modify $ \s -> s { varCount = varCount s + 1 }

initGlobalBlock :: String
initGlobalBlock = ""

initMainBlock :: String
initMainBlock = "\ndefine i32 @main() {\n"

initCodegen :: Codegen
initCodegen = Codegen
  { varCount = 0
  , resultStr = ""
  , globalLookupTable = Map.empty
  , localLookupTable = Map.empty
  , currentScope = Global
  , varIndex = 0
  , globalBlock = initGlobalBlock
  , funcBlocks = []
  , mainBlock = initMainBlock
  , globalVars = []
  , currentBlock = ""
  }

maybeElem :: (Eq a, Foldable t) => a -> t a -> Maybe a
maybeElem x xs = if x `elem` xs then Just x else Nothing

maybeOr :: Maybe a -> Maybe b -> Maybe (Either a b)
maybeOr (Just a) (Just b) = error "maybeOr requires at most 1 of operands to exist"
maybeOr (Just a) Nothing = Just (Left a)
maybeOr Nothing (Just b) = Just (Right b)
maybeOr Nothing Nothing = Nothing

lookup' :: String -> CodegenState (Maybe (Either String Int))
lookup' key = state $ \codegen -> let
  result = maybeOr
    (maybeElem key (globalVars codegen))
    (Map.lookup key (localLookupTable codegen))
  in
    (result, codegen)

{- Returns a state computation with given expression array -}
computeExprs :: [Expr] -> CodegenState ()
computeExprs [x] = genSingleExpr x
computeExprs (x:xs) = genSingleExpr x >> computeExprs xs

codegen :: [Expr] -> Codegen
codegen xs =
  let codegen = execState (computeExprs xs) initCodegen in
  codegen { mainBlock = mainBlock codegen ++ "ret i32 1\n}" }

genSingleExpr :: Expr -> CodegenState ()
genSingleExpr expr =
  case expr of
    Function name args ret ->
      genFunction name args ret
    BinOp op left right -> do
      leftVal <- ensureVar left -- It might not be a Var though.
      rightVal <- ensureVar right
      case op of
        Plus -> genPlusExpr leftVal rightVal
        Minus -> genMinusExpr leftVal rightVal
        Times -> genTimesExpr leftVal rightVal
        Divide -> genDivideExpr leftVal rightVal
    VarDecl name value -> genVarDecl name value
    Var a -> do
      _ <- ensureVar (Var a)
      return ()
    _ -> emptyState' expr

genVarDecl :: String -> Double -> CodegenState ()
genVarDecl name value = modify $ \s ->
  s { globalBlock = globalBlock s ++
                  "@" ++
                  name ++
                  " = weak global i32 " ++
                  show (floor value) ++
                  "\n"
    , globalVars = name : globalVars s
    }

{- Ensure that given expression is a variable. In LLVM, we basically need every
 - argument as variables. If a double is given, we first declare it within the scope
 - then use it. This method exactly does that.
 -}
ensureVar :: Expr -> CodegenState Int
ensureVar expr =
  case expr of
    (Var a) -> do
      var <- lookup' a
      case var of
        Just (Left name) -> loadGlobalVar name
        Just (Right var) -> return var
        Nothing -> error $ "Variable " ++ a ++ " is not defined!"
    (Float num) -> genLocalVar num

genLocalVar :: Double -> CodegenState Int
genLocalVar num = state $ \s ->
      (varIndex s, s { resultStr = resultStr s ++ localVarDecl (varIndex s) num
                          , varIndex = varIndex s + 1
                     })

localVarDecl :: Int -> Double -> String
localVarDecl a b = "%" ++ show a ++ " = add i32 0, " ++ show b ++ "\n"

writeLoadGlobalVar :: String -> CodegenState Int
writeLoadGlobalVar name = state $ \s ->
  (varIndex s, s { currentBlock = currentBlock s ++ "%" ++ show (varIndex s) ++
                                       " = load i32, i32* @" ++ name ++ ", align 4\n" })

increaseVarIndex :: CodegenState ()
increaseVarIndex = modify $ \s ->
  s { varIndex = varIndex s + 1 }

loadGlobalVar :: String -> CodegenState Int
loadGlobalVar name = do
  varIndex <- writeLoadGlobalVar name
  increaseVarIndex
  return varIndex

genOpExpr :: Expr -> Expr -> CodegenState ()
genOpExpr (Float a) (Float b) = emptyState

genPlusExpr :: Int -> Int -> CodegenState ()
genPlusExpr a b = modify $ \s ->
  s { resultStr = resultStr s ++ "%" ++ show (varIndex s) ++ " = add i32 %" ++ show a ++ ", %" ++ show b ++ "\n"
    , varIndex = varIndex s + 1
    }

genMinusExpr :: Int -> Int -> CodegenState ()
genMinusExpr a b = modify $ \s ->
  s { resultStr = resultStr s ++ "%" ++ show (varIndex s) ++ " = sub i32 %" ++ show a ++ ", %" ++ show b ++ "\n"
    , varIndex = varIndex s + 1
    }

genTimesExpr :: Int -> Int -> CodegenState ()
genTimesExpr a b = modify $ \s ->
  s { resultStr = resultStr s ++ "%" ++ show (varIndex s) ++ " = mul i32 %" ++ show a ++ ", %" ++ show b ++ "\n"
    , varIndex = varIndex s + 1
    }

genDivideExpr :: Int -> Int -> CodegenState ()
genDivideExpr a b = modify $ \s ->
  s { resultStr = resultStr s ++ "%" ++ show (varIndex s) ++ " = udiv i32 %" ++ show a ++ ", %" ++ show b ++ "\n"
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
  genFuncBody ret
  genFuncEnd

genFunctionDecl :: Name -> Int -> CodegenState ()
genFunctionDecl name argCount = modify $ \s ->
  s { currentBlock = "\ndefine i32 @" ++ name ++ "(" ++ argTypes ++ ") {\n"
    , currentScope = Local
    }
  where argTypes = sepWithCommas $ replicate argCount "i32"

genFuncBody :: Expr -> CodegenState ()
genFuncBody = genSingleExpr

genFuncLabelVar :: CodegenState ()
genFuncLabelVar = modify $ \s ->
  s { varIndex = varIndex s + 1 } -- Should we record label var?

genFuncArgVar :: Expr -> CodegenState ()
genFuncArgVar (Var arg) = modify $ \s ->
  s { localLookupTable = Map.insert arg (varIndex s) (localLookupTable s)
    , varIndex = varIndex s + 1 }

genFuncArgVars :: [Expr] -> CodegenState ()
genFuncArgVars [] = return ()
genFuncArgVars [x] = genFuncArgVar x
genFuncArgVars (x:xs) = genFuncArgVar x >> genFuncArgVars xs

genFuncEnd :: CodegenState ()
genFuncEnd = modify $ \s ->
  s { varIndex = 0
    , currentScope = Global
    , localLookupTable = emptyLookupTable
    , funcBlocks = (currentBlock s ++ "ret i32 %" ++ show (varIndex s - 1) ++ "\n}\n\n") : funcBlocks s
    , currentBlock = ""
    }

sepWithCommas :: [String] -> String
sepWithCommas [] = ""
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
