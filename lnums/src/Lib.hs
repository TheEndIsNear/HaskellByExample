module Lib
  ( parseArguments
  , readLines
  , numberAllLines
  , numberNonEmptyLines
  , numberAndIncrementNonEmptyLines
  , PadMode(..)
  , padLeft
  , padRight
  , padCenter
  , prettyNumberedLines
  , LineNumberOptions(..)
  ) where

import Data.Char
import Data.Maybe (mapMaybe)

type NumberedLine = (Maybe Int, String)

type NumberedLines = [NumberedLine]

data PadMode
  = PadLeft
  | PadRight
  | PadCenter

data LineNumberOptions
  = ReverseNumbering
  | SkipEmptyLines
  | LeftAlign
  deriving (Eq)

parseArguments :: [String] -> (Maybe FilePath, [LineNumberOptions])
parseArguments args =
  case reverse args of
    [] -> (Nothing, [])
    (filename:options) -> (Just filename, mapMaybe lnOptionsFromString options)

lnOptionsFromString :: String -> Maybe LineNumberOptions
lnOptionsFromString "--reverse" = Just ReverseNumbering
lnOptionsFromString "--skip-empty" = Just SkipEmptyLines
lnOptionsFromString "--left-align" = Just LeftAlign
lnOptionsFromString _ = Nothing

readLines :: FilePath -> IO [String]
readLines filePath = do
  contents <- readFile filePath
  return (lines contents)

isEmpty :: String -> Bool
isEmpty str = null str || all (\s -> not (isPrint s) || isSeparator s) str

isNotEmpty :: String -> Bool
isNotEmpty = not . isEmpty

numberLines :: (String -> Bool) -> (String -> Bool) -> [String] -> NumberedLines
numberLines shouldIncr shouldNumber text =
  let go :: Int -> [String] -> NumberedLines
      go _ [] = []
      go counter (x:xs) =
        let mNumbering =
              if shouldNumber x
                then Just counter
                else Nothing
            newCounter =
              if shouldIncr x
                then counter + 1
                else counter
         in (mNumbering, x) : go newCounter xs
   in go 1 text

numberAllLines :: [String] -> NumberedLines
numberAllLines = numberLines (const True) (const True)

numberNonEmptyLines :: [String] -> NumberedLines
numberNonEmptyLines = numberLines (const True) isNotEmpty

numberAndIncrementNonEmptyLines :: [String] -> NumberedLines
numberAndIncrementNonEmptyLines = numberLines isNotEmpty isNotEmpty

pad :: PadMode -> Int -> String -> String
pad mode n str =
  let diff = n - length str
      centerDiff = div (n - length str) 2
      padding = replicate diff ' '
      centerPadding = replicate centerDiff ' '
   in case mode of
        PadLeft -> padding ++ str
        PadRight -> str ++ padding
        PadCenter -> centerPadding ++ str ++ centerPadding

padLeft :: Int -> String -> String
padLeft = pad PadLeft

padRight :: Int -> String -> String
padRight = pad PadRight

padCenter :: Int -> String -> String
padCenter = pad PadCenter

prettyNumberedLines :: PadMode -> NumberedLines -> [String]
prettyNumberedLines mode lineNums =
  let (numbers, text) = unzip lineNums
      numberStrings = map (maybe "" show) numbers
      maxLength = maximum (map length numberStrings)
      paddedNumbers = map (pad mode maxLength) numberStrings
   in zipWith (\n l -> n ++ " " ++ l) paddedNumbers text
