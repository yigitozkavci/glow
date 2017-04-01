module Main where

import Parser
import Codegen

import Control.Monad.Trans
import System.Console.Haskeline
import Data.Maybe
import System.Process

serializeBlocks :: Codegen -> String
serializeBlocks codegen =
  globalBlock codegen ++
  concat (funcBlocks codegen) ++
  mainBlock codegen

process :: String -> IO ()
process line = do
  let res = parseToplevel line
  case res of
    Left err -> print err
    Right ex -> do
      putStrLn code
      writeFile "out.ll" code
      createProcess (proc "lli" ["out.ll"])
      return ()
      where code = serializeBlocks (codegen ex)

main :: IO ()
main = runInputT defaultSettings loop
  where
  loop = do
    minput <- liftIO $ readFile "in.gl"
    liftIO (process minput) -- >> loop
