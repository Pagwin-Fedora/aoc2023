module Lib
    (combine,
    firstLast,
    digitTransform) where

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

digitTransform :: String -> [Integer]
digitTransform [] = []
digitTransform str | numStart '1' "one" str = 1 : digitTransform (tail str)
digitTransform str | numStart '2' "two" str = 2 : digitTransform (tail str)
digitTransform str | numStart '3' "three" str = 3 : digitTransform (tail str)
digitTransform str | numStart '4' "four" str = 4 : digitTransform (tail str)
digitTransform str | numStart '5' "five" str = 5 : digitTransform (tail str)
digitTransform str | numStart '6' "six" str = 6 : digitTransform (tail str)
digitTransform str | numStart '7' "seven" str = 7 : digitTransform (tail str)
digitTransform str | numStart '8' "eight" str = 8 : digitTransform (tail str)
digitTransform str | numStart '9' "nine" str = 9 : digitTransform (tail str)
digitTransform str = digitTransform $ tail str

numStart :: Char -> String -> String -> Bool
numStart shortHand longHand str | take (length longHand) str == longHand || head str == shortHand = True
numStart _ _ _ = False
firstLast :: [a] -> (a, a)
firstLast list = (head list, last list)

combine :: Show a => (a, a) -> String
combine (a, b) = show a ++ show b
