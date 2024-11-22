module Main (main) where

import Data.Char

main :: IO ()
main = do
  line <- getLine
  putStrLn (map toUpper line)
