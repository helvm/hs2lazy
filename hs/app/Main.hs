module Main where

import HS2Lazy.Facade
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
