module Kaleidoscope.Emit
    ( codegen
    ) where

import Control.Monad.Error
import qualified Data.Map as Map
import LLVM.General.AST
import LLVM.General.Module
import LLVM.General.Context
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Float as F
import qualified LLVM.General.AST.FloatingPointPredicate as FP
import Kaleidoscope.Codegen
import qualified Kaleidoscope.AST as S


toSig :: [String] -> [(AST.Type, AST.Name)]
toSig = map (\ x -> (double, AST.Name x))


codegenTop :: S.Expr -> LLVM ()
codegenTop (S.Function name args body) = do
    define double name (toSig args) $
        createBlocks $ execCodegen $ do
            _ <- addBlock entryBlockName >>= setBlock
            forM_ args $ \ a -> do
                var <- alloca double
                _ <- store var $ LocalReference $ AST.Name a
                assign a var
            cgen body >>= ret

codegenTop (S.Extern name args) =
    external double name (toSig args)

codegenTop expr = do
    define double "main" [] $
        createBlocks $ execCodegen $ do
            _ <- addBlock entryBlockName >>= setBlock
            cgen expr >>= ret


binops :: Map.Map String (AST.Operand -> AST.Operand -> Codegen AST.Operand)
binops = Map.fromList
    [ ("+", \ a b -> instr $ FAdd a b [])
    , ("-", \ a b -> instr $ FSub a b [])
    , ("*", \ a b -> instr $ FMul a b [])
    , ("/", \ a b -> instr $ FDiv a b [])
    , ("<", \ a b -> do { test <- instr $ FCmp FP.ULT a b [] ; uitofp double test })
    ]


cgen :: S.Expr -> Codegen AST.Operand
cgen (S.UnaryOp op a) = do
    cgen $ S.Call ("unary" ++ op) [a]

cgen (S.BinaryOp "=" (S.Var var) val) = do
    a <- getvar var
    cval <- cgen val
    _ <- store a cval
    return cval

cgen (S.BinaryOp op a b) =
    case Map.lookup op binops of
        Nothing -> error "No such operator"
        Just f  -> do
            ca <- cgen a
            cb <- cgen b
            f ca cb

cgen (S.Var x) = getvar x >>= load

cgen (S.Float n) = return $ ConstantOperand $ C.Float (F.Double n)

cgen (S.Call fn args) = do
    largs <- mapM cgen args
    call (ConstantOperand $ C.GlobalReference $ AST.Name fn) largs


-------------------------------------------------------------------------------
-- Compilation
-------------------------------------------------------------------------------

liftError :: ErrorT String IO a -> IO a
liftError = runErrorT >=> either fail return


codegen :: AST.Module -> [S.Expr] -> IO AST.Module
codegen modu fns =
    withContext $ \ context ->
        let newast = runLLVM modu (mapM codegenTop fns)
        in liftError $ withModuleFromAST context newast $ \ m -> do
            llstr <- moduleString m
            putStrLn llstr
            return newast

