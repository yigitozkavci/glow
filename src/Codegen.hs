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
  codegen { mainBlock = mainBlock codegen ++ indent "ret i32 1\n}" }

genSingleExpr :: Expr -> CodegenState ()
genSingleExpr expr =
  case expr of
    Function name args ret ->
      genFunction name args ret
    BinOp op left right -> do
      leftVal <- ensureVar left
      rightVal <- ensureVar right
      genBinOp leftVal rightVal op
    VarDecl name value -> genVarDecl name value
    Var a -> do
      _ <- ensureVar (Var a)
      return ()
    _ -> emptyState' expr

localVarDeclString :: Int -> String -> String
localVarDeclString lhs rhs = indent $ "%" ++ show lhs ++ " = " ++ rhs

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
  (varIndex s, s { currentBlock = currentBlock s ++ localVarDecl (varIndex s) num
                 , varIndex = varIndex s + 1
                 })

indent :: String -> String
indent = ("  " ++)

localVarDecl :: Int -> Double -> String
localVarDecl a b = localVarDeclString a "add i32 0, " ++ show b ++ "\n"

writeLoadGlobalVar :: String -> CodegenState Int
writeLoadGlobalVar name = state $ \s ->
  (varIndex s, s { currentBlock = currentBlock s ++
                                  localVarDeclString (varIndex s) "load i32, i32* @" ++
                                  name ++
                                  ", align 4\n" })

writeToBlock :: String -> CodegenState ()
writeToBlock code = modify $ \s ->
  s { currentBlock = currentBlock s ++ code ++ "\n" }
  
increaseVarIndex :: CodegenState ()
increaseVarIndex = modify $ \s ->
  s { varIndex = varIndex s + 1 }

loadGlobalVar :: String -> CodegenState Int
loadGlobalVar name = do
  varIndex <- writeLoadGlobalVar name
  increaseVarIndex
  return varIndex

opCode :: Op -> String
opCode Plus = "add"
opCode Minus = "sub"
opCode Times = "mul"
opCode Divide = "udiv"

genOpExpr :: Expr -> Expr -> CodegenState ()
genOpExpr (Float a) (Float b) = emptyState

genBinOp :: Int -> Int -> Op -> CodegenState ()
genBinOp left right op = modify $ \s ->
  s { currentBlock = currentBlock s ++ localVarDeclString (varIndex s) (opCode op ++ " i32 %" ++ show left ++ ", %" ++ show right)
    , varIndex = varIndex s + 1 }

-- This is for 'unimplemented' causes only. Should not be shipped into the production
emptyState :: CodegenState ()
emptyState = writeToBlock "[!] non-implemented expression!"

emptyState' :: Expr -> CodegenState ()
emptyState' expr = writeToBlock $ "[!] non-implemented expression:\n[!] " ++ show expr

genFunction :: Name -> [Expr] -> Expr -> CodegenState ()
genFunction name args ret = do
  genFunctionDecl name (length args)
  genFuncArgVars args
  genFuncLabelVar
  genFuncBody ret
  genFuncEnd

setScope :: Scope -> CodegenState ()
setScope scope = modify $ \s ->
  s { currentScope = scope }
  
genFunctionDecl :: Name -> Int -> CodegenState ()
genFunctionDecl name argCount = do
  writeToBlock $ "\ndefine i32 @" ++ name ++ "(" ++ argTypes ++ ") {\n"
  setScope Local
  where argTypes = sepWithCommas $ replicate argCount "i32"

genFuncBody :: Expr -> CodegenState ()
genFuncBody = genSingleExpr

genFuncLabelVar :: CodegenState ()
genFuncLabelVar =
  increaseVarIndex

registerVar :: String -> CodegenState ()
registerVar name = modify $ \s ->
  s { localLookupTable = Map.insert name (varIndex s) (localLookupTable s) }
  
genFuncArgVar :: Expr -> CodegenState ()
genFuncArgVar (Var arg) = do
  registerVar arg
  increaseVarIndex

genFuncArgVars :: [Expr] -> CodegenState ()
genFuncArgVars [] = return ()
genFuncArgVars [x] = genFuncArgVar x
genFuncArgVars (x:xs) = genFuncArgVar x >> genFuncArgVars xs

genFuncEnd :: CodegenState ()
genFuncEnd = modify $ \s ->
  s { varIndex = 0
    , currentScope = Global
    , localLookupTable = emptyLookupTable
    , funcBlocks = (currentBlock s ++ indent ("ret i32 %" ++ show (varIndex s - 1) ++ "\n}\n\n")) : funcBlocks s
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
