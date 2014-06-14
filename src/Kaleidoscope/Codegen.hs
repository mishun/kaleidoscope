{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Kaleidoscope.Codegen
    ( LLVM
    , runLLVM
    , emptyModule
    , define
    , external
    , double
    , Codegen
    , createBlocks
    , entryBlockName
    , execCodegen
    , instr
    , addBlock
    , setBlock
    , assign
    , getvar
    , uitofp
    , call
    , alloca
    , store
    , load
    , ret
    ) where

import Data.Word (Word)
import Data.List (sortBy)
import Data.Function (on)
import qualified Data.Map as Map
import Control.Monad.State (MonadState, State, execState, gets, modify)
import Control.Applicative (Applicative)
import LLVM.General.AST
import qualified LLVM.General.AST.Global as AG
import qualified LLVM.General.AST.CallingConvention as CC


newtype LLVM a = LLVM { unLLVM :: State Module a }
    deriving (Functor, Applicative, Monad, MonadState Module)


runLLVM :: Module -> LLVM a -> Module
runLLVM = flip (execState . unLLVM)


emptyModule :: String -> Module
emptyModule label = defaultModule { moduleName = label }


addDefn :: Definition -> LLVM ()
addDefn d = do
    defs <- gets moduleDefinitions
    modify $ \ s -> s { moduleDefinitions = defs ++ [d] }


define ::  Type -> String -> [(Type, Name)] -> [BasicBlock] -> LLVM ()
define retty label argtys body = addDefn $
    GlobalDefinition $ functionDefaults
        { AG.name        = Name label
        , AG.parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
        , AG.returnType  = retty
        , AG.basicBlocks = body
        }


external ::  Type -> String -> [(Type, Name)] -> LLVM ()
external retty label argtys = addDefn $
    GlobalDefinition $ functionDefaults
        { AG.name        = Name label
        , AG.parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
        , AG.returnType  = retty
        , AG.basicBlocks = []
        }



-- IEEE 754 double
double :: Type
double = FloatingPointType 64 IEEE



type Names = Map.Map String Int

uniqueName :: String -> Names -> (String, Names)
uniqueName nm ns =
  case Map.lookup nm ns of
    Nothing -> (nm,  Map.insert nm 1 ns)
    Just ix -> (nm ++ show ix, Map.insert nm (ix+1) ns)



type SymbolTable = [(String, Operand)]

data CodegenState =
    CodegenState
        { currentBlock :: Name                     -- Name of the active block to append to
        , blocks       :: Map.Map Name BlockState  -- Blocks for function
        , symtab       :: SymbolTable              -- Function scope symbol table
        , blockCount   :: Int                      -- Count of basic blocks
        , count        :: Word                     -- Count of unnamed instructions
        , names        :: Names                    -- Name Supply
        }
    deriving (Show)


data BlockState =
    BlockState
        { idx   :: Int                            -- Block index
        , stack :: [Named Instruction]            -- Stack of instructions
        , term  :: Maybe (Named Terminator)       -- Block terminator
        }
    deriving (Show)



newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
    deriving (Functor, Applicative, Monad, MonadState CodegenState)


sortBlocks :: [(Name, BlockState)] -> [(Name, BlockState)]
sortBlocks = sortBy (compare `on` (idx . snd))


createBlocks :: CodegenState -> [BasicBlock]
createBlocks m = map makeBlock $ sortBlocks $ Map.toList (blocks m)


makeBlock :: (Name, BlockState) -> BasicBlock
makeBlock (l, (BlockState _ s t)) = BasicBlock l s (maketerm t)
    where
        maketerm (Just x) = x
        maketerm Nothing = error $ "Block has no terminator: " ++ (show l)


entryBlockName :: String
entryBlockName = "entry"


emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing


emptyCodegen :: CodegenState
emptyCodegen = CodegenState (Name entryBlockName) Map.empty [] 1 0 Map.empty


execCodegen :: Codegen a -> CodegenState
execCodegen m = execState (runCodegen m) emptyCodegen


fresh :: Codegen Word
fresh = do
    i <- gets count
    modify $ \ s -> s { count = 1 + i }
    return $ i + 1


instr :: Instruction -> Codegen Operand
instr ins = do
    n <- fresh
    let ref = UnName n
    blk <- current
    let i = stack blk
    modifyBlock (blk { stack = i ++ [ref := ins] } )
    return $ LocalReference ref


terminator :: Named Terminator -> Codegen (Named Terminator)
terminator trm = do
    blk <- current
    modifyBlock (blk { term = Just trm })
    return trm



addBlock :: String -> Codegen Name
addBlock bname = do
    bls <- gets blocks
    ix <- gets blockCount
    nms <- gets names
    let new = emptyBlock ix
        (qname, supply) = uniqueName bname nms
    modify $ \ s -> s { blocks = Map.insert (Name qname) new bls
                      , blockCount = ix + 1
                      , names = supply
                      }
    return (Name qname)


setBlock :: Name -> Codegen Name
setBlock bname = do
    modify $ \ s -> s { currentBlock = bname }
    return bname


modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
    active <- gets currentBlock
    modify $ \s -> s { blocks = Map.insert active new (blocks s) }


current :: Codegen BlockState
current = do
    c <- gets currentBlock
    blks <- gets blocks
    case Map.lookup c blks of
        Just x  -> return x
        Nothing -> error $ "No such block: " ++ show c



assign :: String -> Operand -> Codegen ()
assign var x = do
    lcls <- gets symtab
    modify $ \ s -> s { symtab = [(var, x)] ++ lcls }


getvar :: String -> Codegen Operand
getvar var = do
    syms <- gets symtab
    case lookup var syms of
        Just x  -> return x
        Nothing -> error $ "Local variable not in scope: " ++ show var


uitofp :: Type -> Operand -> Codegen Operand
uitofp ty a = instr $ UIToFP a ty []


call :: Operand -> [Operand] -> Codegen Operand
call fn args = instr $ Call False CC.C [] (Right fn) (map (\ x -> (x, [])) args) [] []


alloca :: Type -> Codegen Operand
alloca ty = instr $ Alloca ty Nothing 0 []


store :: Operand -> Operand -> Codegen Operand
store ptr val = instr $ Store False ptr val Nothing 0 []


load :: Operand -> Codegen Operand
load ptr = instr $ Load False ptr Nothing 0 []


ret :: Operand -> Codegen (Named Terminator)
ret val = terminator $ Do $ Ret (Just val) []

