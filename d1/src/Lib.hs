module Lib
    (onlyDigits) where
import Data.Char (toLower)
import Data.String.Utils (replace)

isDigit :: Char -> Bool
isDigit '0' = True
isDigit '1' = True
isDigit '2' = True
isDigit '3' = True
isDigit '4' = True
isDigit '5' = True
isDigit '6' = True
isDigit '7' = True
isDigit '8' = True
isDigit '9' = True
isDigit _ = False

onlyDigits :: String -> [Char]
onlyDigits = filter isDigit-- . digitTransform . map toLower

nums = map (map toLower) ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
digitTransform :: String -> String
digitTransform = 
    replace (nums !! 0) (show 0) .
    replace (nums !! 1) (show 1) .
    replace (nums !! 2) (show 2) .
    replace (nums !! 3) (show 3) .
    replace (nums !! 4) (show 4) .
    replace (nums !! 5) (show 5) .
    replace (nums !! 6) (show 6) .
    replace (nums !! 7) (show 7) .
    replace (nums !! 8) (show 8) .
    replace (nums !! 9) (show 9)
