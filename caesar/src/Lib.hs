module Lib (caesar, rot13, rot135) where

type Alphabet = [Char]

upperAlphabet :: Alphabet
upperAlphabet = ['A' .. 'Z']

lowerAlphabet :: Alphabet
lowerAlphabet = ['a' .. 'z']

digits :: Alphabet
digits = ['0' .. '9']

isUpper :: Char -> Bool
isUpper char = char `elem` upperAlphabet

isLower :: Char -> Bool
isLower char = char `elem` lowerAlphabet

isDigit :: Char -> Bool
isDigit char = char `elem` digits

isMisc :: Char -> Bool
isMisc char = char `notElem` upperAlphabet ++ lowerAlphabet ++ digits

indexOf :: Char -> Alphabet -> Int
indexOf _ch [] = undefined
indexOf ch (x : xs) = if x == ch then 0 else 1 + indexOf ch xs

alphabetRot :: Alphabet -> Int -> Char -> Char
alphabetRot alphabet n ch = alphabet !! ((indexOf ch alphabet + n) `mod` length alphabet)

upperRot :: Int -> Char -> Char
upperRot = alphabetRot upperAlphabet

lowerRot :: Int -> Char -> Char
lowerRot = alphabetRot lowerAlphabet

digitRot :: Int -> Char -> Char
digitRot = alphabetRot digits

rotChar :: Int -> Char -> Char
rotChar n ch
  | isLower ch = lowerRot n ch
  | isUpper ch = upperRot n ch
  | isDigit ch = digitRot n ch
  | isMisc ch = ch
  | otherwise = ch

caesar :: Int -> String -> String
caesar n = map (rotChar n)

rot13 :: String -> String
rot13 = caesar 13

rot135 :: String -> String
rot135 = map rotSingleChar
  where
    rotSingleChar ch
      | isDigit ch = rotChar 5 ch
      | isLower ch || isUpper ch = rotChar 13 ch
      | isMisc ch = ch
      | otherwise = ch
