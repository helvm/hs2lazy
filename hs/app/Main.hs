module Main where

import Data.Char (toLower)
import HS2Lazy.Builtin (expandBltin)
import HS2Lazy.Compiler (expandCon, programToExpr, skiCompile)
import HS2Lazy.Facade
import qualified HS2Lazy.Lexer as L
import HS2Lazy.Optimizer (optimizeExpr)
import HS2Lazy.PPrint (showProgram)
import qualified HS2Lazy.Parser as P
import HS2Lazy.PatComp (compilePatternMatch)
import qualified HS2Lazy.Static as S
import HS2Lazy.Syntax
import qualified HS2Lazy.Type as T
import System.Environment
import System.IO

main :: IO ()
main = do
  source <- argf
  ski <- runIO source
  putStrLn $ ski

argf :: IO String
argf = do
  argv <- getArgs
  if argv == []
    then getContents
    else do
      conts <- mapM getFileContents argv
      return (concat conts)

getFileContents :: String -> IO String
getFileContents fname = do
  handle <- openFile fname ReadMode
  hGetContents handle
