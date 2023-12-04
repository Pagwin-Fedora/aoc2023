module Lib
    (Row(..),
    numsOnBoard,
    symsOnBoard,
    adjacentOnly,
    grabNums) where
import Data.Foldable (Foldable(foldl'))
import Data.Maybe (listToMaybe)
import Data.Bifunctor (Bifunctor(second))
import Data.Char (isDigit)

data NumRange = NumRange {start :: Coordinate, rangeLength :: Int}

data Coordinate = Coordinate {xCoord :: Int, yCoord :: Int}

data Symbol = Symbol {symbolLocation :: Coordinate, symbolChar :: Char}

newtype Row = Row String

unRow :: Row -> String
unRow (Row row) = row

unSymbol :: Symbol -> Char
unSymbol (Symbol _ char) = char

coordIndex :: [Row] -> Coordinate -> Char
coordIndex rowList (Coordinate x y) = unRow (rowList !! y) !! (length rowList - x-1)

numsOnBoard :: [Row] -> [NumRange]
numsOnBoard = foldl' (++) [] . map (numsInString . second unRow) . zip [0..]

safeHead :: [a] -> Maybe a
safeHead = listToMaybe

numsInString :: (Int, String) -> [NumRange]
numsInString (_, []) = []
numsInString (rowNum, currChar:rowRem) = let
    tailEnd = numsInString (rowNum, rowRem) in
    if not $ isDigit currChar then
        tailEnd
    else let maybeHead = safeHead tailEnd in
        case maybeHead of
            Nothing -> NumRange (Coordinate (length rowRem) rowNum) 1 : tailEnd
            Just (NumRange (Coordinate x y) len) -> (
                if y == rowNum then 
                    NumRange (Coordinate (x+1) y) (len+1) 
                else 
                    NumRange (Coordinate (length rowRem) rowNum) 1
                ) : tailEnd


symsOnBoard :: [Row] -> [Symbol]
symsOnBoard = filter ((/= '.') . unSymbol) . foldl' (++) [] . map (\(rowNum,row) -> map (\(columnNum, char) -> Symbol (Coordinate columnNum rowNum) char) $ zip [0..] $ unRow row) . zip [0..]

adjacent :: NumRange -> Symbol -> Bool
adjacent range (Symbol (Coordinate x y) _) = (x >= (xCoord (start range)-1)) && (x <= (xCoord (start range)+rangeLength range+1)) && (1>= abs (y-yCoord (start range)))

adjacentOnly :: [NumRange] -> [Symbol] -> [NumRange]
adjacentOnly nums syms = filter (\num -> any (adjacent num) syms) nums

grabNums :: [Row] -> [NumRange] -> [Int]
grabNums rows = map (\(NumRange start rangeLength) -> read $ reverse $ take rangeLength $ unRow (rows !! (yCoord start)))
