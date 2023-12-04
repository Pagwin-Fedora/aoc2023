module Main (main) where
import Lib
import System.IO.Utils (hGetLines)
import System.IO (stdin)

main :: IO ()
main = do
    board <- map Row <$> hGetLines stdin
    let nums = numsOnBoard board
    let syms = symsOnBoard board
    print $ sum $ grabNums board $ adjacentOnly nums syms
    return ()
