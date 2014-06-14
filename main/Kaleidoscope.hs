module Main
    ( main
    ) where

import Control.Monad.Trans
import System.Environment (getArgs)
import System.Console.Haskeline
import qualified LLVM.General.AST as AST
import Kaleidoscope.Parser
import Kaleidoscope.Codegen
import Kaleidoscope.Emit


initModule :: AST.Module
initModule = emptyModule "my cool jit"


process :: AST.Module -> String -> IO (Maybe AST.Module)
process modo source = do
    let res = parseToplevel source
    case res of
        Left err -> print err >> return Nothing
        Right ex -> do
            ast <- codegen modo ex
            return $ Just ast


processFile :: String -> IO (Maybe AST.Module)
processFile fname = readFile fname >>= process initModule


repl :: IO ()
repl =
    let loop mod' = do
            minput <- getInputLine "ready> "
            case minput of
                Nothing    -> outputStrLn "Goodbye."
                Just input -> do
                    modn <- liftIO $ process mod' input
                    case modn of
                        Just modn' -> loop modn'
                        Nothing    -> loop mod'
    in runInputT defaultSettings (loop initModule)


main :: IO ()
main = do
    args <- getArgs
    case args of
        []      -> repl
        [fname] -> processFile fname >> return ()
        _       -> error "bad command line"

