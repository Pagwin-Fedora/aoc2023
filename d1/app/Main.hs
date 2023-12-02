module Main (main) where

import Lib
import Data.List (elemIndex)
import Data.Maybe (isJust)
import System.IO.Utils
import System.IO (stdin)
main :: IO ()
main = processLines >>= putStrLn

processLines :: IO String
processLines = show . sum . map processLine <$> hGetLines stdin

processLine :: String -> Integer
processLine "" = 0
processLine line = read . combine . firstLast . digitTransform $ line

