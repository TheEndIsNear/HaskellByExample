module Main
  ( main
  ) where

import Lib
  ( LineNumberOptions(..)
  , PadMode(..)
  , numberAllLines
  , numberNonEmptyLines
  , parseArguments
  , prettyNumberedLines
  , readLines
  )
import System.Environment

printHelpText :: String -> IO ()
printHelpText msg = do
  putStrLn (msg ++ "\n")
  progName <- getProgName
  putStrLn ("Usage: " ++ progName ++ " <filename>")
  putStrLn "\n"
  putStrLn "Options:"
  putStrLn "    --reverse      - Reverse the numbering"
  putStrLn "    --skip-empty   - Skip numbering empty lines"
  putStrLn "    --left-align   - Use left-aligned line numbers"

main :: IO ()
main = do
  cliArgs <- getArgs
  let (mFilePath, options) = parseArguments cliArgs
      numberFunction =
        if SkipEmptyLines `elem` options
          then numberNonEmptyLines
          else numberAllLines
      padMode =
        if LeftAlign `elem` options
          then PadRight
          else PadLeft
      go filePath = do
        fileLines <- readLines filePath
        let numbered = numberFunction fileLines
            prettyNumbered = prettyNumberedLines padMode numbered
            revNumbered = numberFunction $ reverse fileLines
            revPretty = reverse (prettyNumberedLines padMode revNumbered)
        mapM_
          putStrLn
          (if ReverseNumbering `elem` options
             then revPretty
             else prettyNumbered)
  maybe (printHelpText "Missing filename") go mFilePath
