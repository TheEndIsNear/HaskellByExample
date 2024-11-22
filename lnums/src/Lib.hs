module Lib
    ( parseArguments 
    ) where

parseArguments :: [String] -> Maybe FilePath
parseArguments [filepath] = Just filepath
parseArguments _ = Nothing

