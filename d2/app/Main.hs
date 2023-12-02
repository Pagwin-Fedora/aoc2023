module Main where

import System.IO.Utils
import Data.List.Utils
import System.IO (stdin)
import Data.Bifunctor (Bifunctor(bimap))
import Data.Foldable (find)
import Data.Maybe (fromJust)
import Data.Char (isDigit)

main :: IO ()
main = handleInput >>= putStrLn

handleInput :: IO String
handleInput = show . sum . map processLine <$> hGetLines stdin

colorMaxMapping :: [(String, Integer)]
colorMaxMapping = [("blue", 14), ("green", 13), ("red", 12)]

processLine :: String -> Integer
processLine line = if validGame then gameNum line else 0
    where validGame = all processRound $ split ";" line

processRound :: String -> Bool
processRound gameRound = (all (processElem colorMaxMapping) . split "," . choose (take 4 gameRound == "Game") (dropWhile (== ':') . dropWhile (/= ':')) id) gameRound

choose :: Bool -> a -> a -> a
choose cond a b = if cond then a else b

processElem :: [(String, Integer)] -> String -> Bool
processElem mapping element = num <= maxCount
    where
    (num, color) = parseCountColor element
    maxCount = maxCount' mapping color

parseCountColor :: String -> (Integer, String)
parseCountColor element = bimap (read . takeWhile isDigit) (drop 1) ((span (/= ' ') . dropWhile (not . isDigit))  element)

maxCount' :: [(String, Integer)] -> String -> Integer
maxCount' mapping color = snd . fromJust $ find ((== color) . fst) mapping 

gameNum :: String -> Integer
gameNum =  read . takeWhile isDigit . dropWhile (not . isDigit)
