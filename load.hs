module Load 
( getDrawnSudokuMap
, getSudokuState
) where

import Utils

getDrawnSudokuMap :: [[(Int, Int, Char, Char)]] -> [[String]]
getDrawnSudokuMap coords = drawTopRow ++ (map (\row -> [drawZoneRow row coords, drawBarrierRow row coords]) coords)

drawTopRow :: [[String]]
drawTopRow = [["|" ++ (replicate (9*5) '-') ++ "|"]]

drawBarrierRow :: [(Int, Int, Char, Char)] -> [[(Int, Int, Char, Char)]] -> String
drawBarrierRow row coords = "|" ++ (concat $ map (\coord -> getBarrierRow coord coords) row) ++ "|"

drawZoneRow :: [(Int, Int, Char, Char)] -> [[(Int, Int, Char, Char)]] -> String
drawZoneRow row coords =  "|" ++ (concat $ (map (\coord -> getZoneRow coord coords) row)) ++ "|"

getZoneRow :: (Int, Int, Char, Char) -> [[(Int, Int, Char, Char)]] -> String
getZoneRow (x, y, zone, value) coords
  | x == 8 = "  " ++ [value] ++ "  "
  | getZone (x + 1) y coords /= zone = "  " ++ [value] ++ " |"
  | otherwise = "  " ++ [value] ++ "  "

getBarrierRow :: (Int, Int, Char, Char) -> [[(Int, Int, Char, Char)]] -> String
getBarrierRow (x, y, zone, value) coords
  | y == 8 = "-----"
  | x == 8 && getZone x (y + 1) coords == zone  = "     "
  | x == 8 && getZone x (y + 1) coords /= zone = "-----"
  | getZone x (y + 1) coords /= zone = "-----"
  | getZone (x + 1) y coords /= zone = "    |"
  | otherwise = "     "

getSudokuState mapRows stateRows = map (\(y, row) -> getSudokuRowState y row stateRows) $ zip[0..] mapRows

getSudokuRowState :: Int -> String -> [String] -> [(Int, Int, Char, Char)]
getSudokuRowState y row states = map (\(x, zone) -> (x, y, zone, getSavedState x y states)) $ zip [0..] row