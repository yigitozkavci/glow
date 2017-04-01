module Main where

import Parser
import Codegen

import Control.Monad.Trans
import System.Console.Haskeline

printBlocks :: Codegen -> String
printBlocks codegen =
  globalBlock codegen ++
  concat (funcBlocks codegen) ++
  mainBlock codegen

process :: String -> IO ()
process line = do
  let res = parseToplevel line
  case res of
    Left err -> print err
    Right ex -> putStrLn . printBlocks $ codegen ex

main :: IO ()
main = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "ready> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> liftIO (process input) >> loop
