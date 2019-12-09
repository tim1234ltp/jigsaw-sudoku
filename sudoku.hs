import System.IO
import Debug.Trace
import Data.Char


trd :: (Int, Int, Char, Char) -> Char
trd (x, y, zone, value) = zone

fth :: (Int, Int, Char, Char) -> Char
fth (x, y, zone, value) = value

getZone :: Int -> Int -> [[(Int, Int, Char, Char)]] -> Char
getZone x y coords = trd ((coords !! y) !! x)

getValue:: Int -> Int -> [[(Int, Int, Char, Char)]] -> Char
getValue x y coords = trd ((coords !! y) !! x)

getSavedState :: Int -> Int -> [String] -> Char
getSavedState x y states = ((states !! y) !! x)

main = do 
    contents <- readFile "map2.txt"
    let allRows = lines contents
        mapRows = take (9) allRows
        stateRows = drop (9) allRows
        positions = map (\(y, row) -> getMapPositions y row stateRows) $ zip[0..] mapRows
        sudokuMap = concat $ (drawMap positions)
    putStrLn (unlines sudokuMap)

drawMap :: [[(Int, Int, Char, Char)]] -> [[String]]
drawMap coords = drawTopRow ++ (map (\row -> [drawZoneRow row coords, drawBarrierRow row coords]) coords)

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

getMapPositions :: Int -> String -> [String] -> [(Int, Int, Char, Char)]
getMapPositions y row states = map (\(x, zone) -> (x, y, zone, getSavedState x y states)) $ zip [0..] row

