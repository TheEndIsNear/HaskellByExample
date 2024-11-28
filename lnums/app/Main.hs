module Main (main) where

import Lib (parseArguments, prettyNumberedLines, readLines, numberAllLines, PadMode(..))
import System.Environment

printHelpText :: String -> IO ()
printHelpText msg = do
  putStrLn (msg ++ "\n")
  progName <- getProgName
  putStrLn ("Usage: " ++ progName ++ " <filename>")

main :: IO ()
main = do
  cliArgs <- getArgs
  let mFilePath = parseArguments cliArgs
  maybe
    (printHelpText "Missing filename")
    ( \filePath -> do
        fileLines <- readLines filePath
        let numbered = numberAllLines fileLines
            prettyNumbered = prettyNumberedLines PadLeft numbered
        mapM_ putStrLn prettyNumbered
       )
    mFilePath
