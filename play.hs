module Play
( setSudokuState
, isMoveValid
, isGameOver
) where

import Utils

setSudokuState :: Int -> Int -> Char -> [[(Int, Int, Char, Char)]] -> [[(Int, Int, Char, Char)]] 
setSudokuState x y newValue positions = map (\row -> setStateInRow x y newValue row) positions

setStateInRow :: Int -> Int -> Char -> [(Int, Int, Char, Char)] -> [(Int, Int, Char, Char)]
setStateInRow x y newValue row = map (\position -> returnState x y newValue position) row

returnState :: Int -> Int -> Char -> (Int, Int, Char, Char) -> (Int, Int, Char, Char)
returnState x y newValue position
  | x == getPosX position && y == getPosY position = (x, y, getPosZone position, newValue)
  | otherwise = position

isMoveValid :: Int -> Int -> Char -> [[(Int, Int, Char, Char)]] -> Bool
isMoveValid x y val positions = (isColumnValid val column) && (isRowValid val row) && (isZoneValid val zone)
  where row = positions !! y
        zone = concat $ map (\row ->  (filterZoneMembers row (getZone x y positions)) ) positions
        column = map (\row -> row !! x) positions

filterZoneMembers :: [(Int, Int, Char, Char)] -> Char -> [(Int, Int, Char, Char)]
filterZoneMembers row targetZone  = filter (\(x, y, zone, val) -> zone == targetZone) row

isRowValid :: Char -> [(Int, Int, Char, Char)] -> Bool
isRowValid val row =  (length (filter (\(x, y, zone, value) -> value == val) row)) < 2

isColumnValid :: Char -> [(Int, Int, Char, Char)] -> Bool
isColumnValid val column =  (length (filter (\(x, y, zone, value) -> value == val) column)) < 2

isZoneValid :: Char -> [(Int, Int, Char, Char)] -> Bool
isZoneValid val zone =  (length (filter (\(x, y, zone, value) -> value == val) zone)) < 2

isGameOver :: [[(Int, Int, Char, Char)]] -> Bool
isGameOver positions = (length (concat $ map (\row -> (filter (\(x, y, zone, value) -> value == '.') row)) positions)) == 0 