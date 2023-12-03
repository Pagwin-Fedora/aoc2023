module Main where

import System.IO.Utils
import Data.List.Utils
import System.IO (stdin)
import Data.Bifunctor (Bifunctor(bimap))
import Data.Foldable (Foldable (foldl'))
import Data.Char (isDigit)

main :: IO ()
main = handleInput >>= putStrLn

handleInput :: IO String
handleInput = show . sum . map processLine <$> hGetLines stdin

colorMaxMapping :: [(String, Integer)]
colorMaxMapping = [("blue", 14), ("green", 13), ("red", 12)]

processLine :: String -> Integer
processLine = cProd . foldl' pickMax (Colors 0 0 0) . map processRound . split ";"

processRound :: String -> Colors
processRound gameRound = combineColors . map processElem . split "," . choose (take 4 gameRound == "Game") (dropWhile (== ':') . dropWhile (/= ':')) id $ gameRound

choose :: Bool -> a -> a -> a
choose cond a b = if cond then a else b

data Color = Red Integer | Green Integer | Blue Integer

processElem :: String -> Color
processElem element = case color of
    "red" -> Red num
    "green" -> Green num
    "blue" -> Blue num
    remaining -> error $ "Error invalid color: " ++ remaining
    where
    (num, color) = parseCountColor element

parseCountColor :: String -> (Integer, String)
parseCountColor element = bimap (read . takeWhile isDigit) (drop 1) ((span (/= ' ') . dropWhile (not . isDigit))  element)

data Colors = Colors {redPart :: Integer, greenPart :: Integer, bluePart ::Integer}

pickMax :: Colors -> Colors -> Colors
pickMax (Colors r1 g1 b1) (Colors r2 g2 b2) = Colors (max r1 r2) (max g1 g2) (max b1 b2)

unRed :: [Color] -> Integer
unRed (Red val: _) = val
unRed (_:xs) = unRed xs
unRed [] = 0

unGreen :: [Color] -> Integer
unGreen (Green val: _) = val
unGreen (_:xs) = unGreen xs
unGreen [] = 0

unBlue :: [Color] -> Integer
unBlue (Blue val: _) = val
unBlue (_:xs) = unBlue xs
unBlue [] = 0


combineColors :: [Color] -> Colors
combineColors cList = Colors red green blue
    where 
    red = unRed cList
    green = unGreen cList
    blue = unBlue cList

cProd :: Colors -> Integer
cProd (Colors r g b) = r * g * b

instance Show Colors where
    show (Colors r g b) = "Colors " ++ show r ++ " " ++ show g ++ " " ++ show b
instance Show Color where
    show (Red r)    = "Red " ++ show r
    show (Green g)  = "Green " ++ show g
    show (Blue b)   = "Blue " ++ show b
