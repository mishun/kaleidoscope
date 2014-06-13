module Main
    ( main
    ) where

import Control.Monad.Trans
import System.Console.Haskeline
import Kaleidoscope.Parser


process :: String -> IO ()
process line =
  let res = parseToplevel line
  in case res of
    Left err -> print err
    Right ex -> mapM_ print ex


main :: IO ()
main =
    let loop = do
            minput <- getInputLine "ready> "
            case minput of
                Nothing    -> outputStrLn "Goodbye."
                Just input -> liftIO (process input) >> loop
    in runInputT defaultSettings loop

