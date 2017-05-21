{-# LANGUAGE OverloadedStrings #-}

module Emit where

import LLVM.General.Module
import LLVM.General.Context
import LLVM.General.PassManager
import LLVM.General.Transforms (Pass(PromoteMemoryToRegister))

import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Instruction as I
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Float as F
import qualified LLVM.General.AST.FloatingPointPredicate as FP

import qualified LLVM.General.ExecutionEngine as EE
import Data.Word
import Data.Int
import Control.Monad.Except
import Control.Applicative
import qualified Data.Map as Map
import Foreign.Ptr

import Codegen
import qualified Syntax as S

one = cons $ C.Float (F.Double 1.0)
zero = cons $ C.Float (F.Double 0.0)
true = one
false = zero

toSig :: [S.TypedName] -> [(AST.Type, AST.Name)]
toSig = map (\(S.TypedName type' name) -> (S.declToType type', AST.Name name))

toDoubleSig :: [String] -> [(AST.Type, AST.Name)]
toDoubleSig = map (\x -> (double, AST.Name x))

processArg :: S.TypedName -> Codegen ()
processArg (S.TypedName rawType name) = do
  let argType = S.declToType rawType
  var <- alloca argType
  store var (local argType (AST.Name name))
  pointer <- load var
  assign name pointer

codegenTop :: S.Expr -> LLVM ()
codegenTop (S.Function typeDecl name args body) =
  define (S.declToType typeDecl) name fnargs bls
  where
    fnargs = toSig args
    bls = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      forM_ args processArg
      cgen body >>= ret

codegenTop (S.Extern name args) =
  external double name fnargs
  where fnargs = toDoubleSig args

codegenTop (S.BinaryDef typeDecl name args body) =
  codegenTop $ S.Function typeDecl ("binary" ++ name) args body

codegenTop (S.UnaryDef typeDecl name arg body) =
  codegenTop $ S.Function typeDecl ("unary" ++ name) [arg] body

codegenTop exp =
  define double "main" [] blks
  where
    blks = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      cgen exp >>= ret



-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

lt :: AST.Operand -> AST.Operand -> Codegen AST.Operand
lt a b = do
  test <- fcmp FP.ULT a b
  uitofp double test

binops :: Map.Map String (AST.Operand -> AST.Operand -> Codegen AST.Operand)
binops = Map.fromList [
      ("+", fadd)
    , ("-", fsub)
    , ("*", fmul)
    , ("/", fdiv)
    , ("<", lt)
  ]

intOperand :: Integer -> AST.Operand
intOperand val = cons $ C.Int 32 $ toInteger val

cgen :: S.Expr -> Codegen AST.Operand

-- Types
cgen (S.Float n) = return $ cons $ C.Float (F.Double n)

cgen (S.DoubleArray elems) = do
  i <- alloca (array (fromIntegral . length $ elems) double)
  let arr = cons $ C.Array double (map (C.Float . F.Double) elems)
  store i arr
  instr doublePtr $ I.GetElementPtr True i [intOperand 0, intOperand 0] []

cgen (S.IntArray elems) = do
  i <- alloca (array (fromIntegral . length $ elems) int)
  let arr = cons $ C.Array int (map (C.Int 32) elems)
  store i arr
  instr intPtr $ I.GetElementPtr True i [intOperand 0, intOperand 0] []

cgen (S.Integer val) = return $ cons $ C.Int 32 val

-- Memory
cgen (S.Var x) = getvar x >>= load

cgen (S.ArrAccess var index) = do
  arrOperand <- getvar var
  let indice = cons $ C.Int 32 (toInteger index)
  pointer <- instr double $ I.GetElementPtr True arrOperand [indice] []
  load pointer

-- Allocations
cgen (S.Call fn args) = do
  largs <- mapM cgen args
  call (externf (AST.Name fn)) largs

cgen (S.If cond tr fl) = do
  ifthen <- addBlock "if.then"
  ifelse <- addBlock "if.else"
  ifexit <- addBlock "if.exit"

  -- %entry
  -----------------
  cond <- cgen cond
  test <- fcmp FP.ONE false cond
  cbr test ifthen ifelse

  -- if.then
  setBlock ifthen
  trval <- cgen tr
  br ifexit
  ifthen <- getBlock

  -- if.else
  setBlock ifelse
  flval <- cgen fl
  br ifexit
  ifelse <- getBlock

  -- if.exit
  setBlock ifexit
  phi double [(trval, ifthen), (flval, ifelse)]

cgen (S.For ivar start cond step body) = do
  forloop <- addBlock "for.loop"
  forexit <- addBlock "for.exit"

  -- %entry
  i <- alloca double
  istart <- cgen start
  stepval <- cgen step

  store i istart
  assign ivar i
  br forloop

  -- for.loop
  setBlock forloop
  cgen body
  ival <- load i
  inext <- fadd ival stepval
  store i inext

  cond <- cgen cond
  test <- fcmp FP.ONE false cond
  cbr test forloop forexit

  -- for.exit
  setBlock forexit
  return zero

cgen (S.Let a b@(S.DoubleArray elems) c) = do
  val <- cgen b
  i <- alloca (ptr double)
  store i val
  pointer <- load i
  assign a pointer
  cgen c

cgen (S.Let a b@(S.IntArray elems) c) = do
  val <- cgen b
  i <- alloca (ptr int)
  store i val
  pointer <- load i
  assign a pointer
  cgen c

cgen (S.UnaryOp op a) =
    cgen (S.Call ("unary" ++ op) [a])

cgen (S.Let a b c) = do
  val <- cgen b
  i <- alloca (oType val)
  store i val
  assign a i
  cgen c

cgen (S.BinaryOp "<-" (S.Var var) val) = do
  a <- getvar var
  cval <- cgen val
  store a cval
  return cval

cgen (S.BinaryOp op a b) =
  case Map.lookup op binops of
    Just f -> do
      ca <- cgen a
      cb <- cgen b
      f ca cb
    Nothing -> cgen (S.Call ("binary" ++ op) [a, b])

cgen other = error $ "Code generation for " ++ show other ++ " is not defined."
-------------------------------------------------------------------------------
-- Compilation
-------------------------------------------------------------------------------

passes :: PassSetSpec
passes = defaultPassSetSpec { transforms = [PromoteMemoryToRegister] }

foreign import ccall "dynamic" haskFun :: FunPtr (IO Double) -> IO Double

run :: FunPtr a -> IO Double
run fn = haskFun (castFunPtr fn :: FunPtr (IO Double))

runJIT :: AST.Module -> IO (Either String AST.Module)
runJIT mod =
  withContext $ \context ->
    jit context $ \executionEngine ->
      runExceptT $ withModuleFromAST context mod $ \m ->
        withPassManager passes $ \pm -> do
          runPassManager pm m
          optmod <- moduleAST m
          s <- moduleLLVMAssembly m
          putStrLn s

          EE.withModuleInEngine executionEngine m $ \ee -> do
            mainfn <- EE.getFunction ee (AST.Name "main")
            case mainfn of
              Just fn -> do
                res <- run fn
                putStrLn $ "Evaluated to: " ++ show res
              Nothing -> do
                putStrLn "Function could not be evaluated."
                return ()

          return optmod

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

codegen :: AST.Module -> [S.Expr] -> IO AST.Module
codegen mod fns = withContext $ \context ->
  liftError $ withModuleFromAST context newast $ \m -> do
    llstr <- moduleLLVMAssembly m
    -- putStrLn llstr
    return newast
  where
    modn    = mapM codegenTop fns
    newast  = runLLVM mod modn

jit :: Context -> (EE.MCJIT -> IO a) -> IO a
jit context = EE.withMCJIT context optlevel model ptrelim fastins
  where
    optlevel = Just 2
    model    = Nothing
    ptrelim  = Nothing
    fastins  = Nothing
