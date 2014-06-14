module Kaleidoscope.JIT where

import Foreign.Ptr (FunPtr, castFunPtr)
import Control.Monad.Error (runErrorT)
import LLVM.General.Context (Context, withContext)
import LLVM.General.Module
import qualified LLVM.General.AST as AST
import LLVM.General.PassManager
import qualified LLVM.General.ExecutionEngine as EE


foreign import ccall "dynamic" haskFun :: FunPtr (IO Double) -> (IO Double)


run :: FunPtr a -> IO Double
run fn = haskFun (castFunPtr fn :: FunPtr (IO Double))


jit :: Context -> (EE.JIT -> IO a) -> IO a
jit c =
    let optlevel = 0
    in EE.withJIT c optlevel


passes :: PassSetSpec
passes = defaultCuratedPassSetSpec { optLevel = Just 3 }


runJIT :: AST.Module -> IO (Either String AST.Module)
runJIT llvmModule =
    withContext $ \ context ->
        jit context $ \ executionEngine ->
            runErrorT $ withModuleFromAST context llvmModule $ \ m ->
                withPassManager passes $ \ pm -> do
                    -- Optimization Pass
                    _ <- runPassManager pm m

                    optmod <- moduleAST m
                    s <- moduleString m --moduleLLVMAssembly m
                    putStrLn s

                    EE.withModuleInEngine executionEngine m $ \ ee -> do
                        mainfn <- EE.getFunction ee (AST.Name "main")
                        case mainfn of
                            Nothing -> return ()
                            Just fn -> do
                                res <- run fn
                                putStrLn $ "Evaluated to: " ++ show res

                    return optmod

