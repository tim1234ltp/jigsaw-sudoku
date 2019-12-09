import System.IO
import Debug.Trace


--data MapPoint = MapPoint Int Int Int String deriving (Show)
--data MapSymbol = VerticalBarrier Coords | HorizontalBarrier Coords | EmptyArea Coords | Number Coords Int deriving (Show)

trd :: (Int, Int, Char) -> Char
trd (x, y, value) = value

getValue :: Int -> Int -> [[(Int, Int, Char)]] -> Char
getValue x y coords = trd ((coords !! y) !! x)


main = do 
    contents <- readFile "map.txt"
    let allRows = lines contents
    let coords = map (\(y, row) -> getPoints y row) $ zip[0..] allRows

    let map = concat $ drawMap coords 
    --print map
    putStrLn (unlines map)

drawMap :: [[(Int, Int, Char)]] -> [[String]]
drawMap coords = drawTopRow ++ (map (\row -> [drawValueRow row coords, drawBarrierRow row coords]) coords)

drawTopRow :: [[String]]
drawTopRow = [["|" ++ (replicate (9*5) '-') ++ "|"]]

drawBarrierRow :: [(Int, Int, Char)] -> [[(Int, Int, Char)]] -> String
drawBarrierRow row coords = "|" ++ (concat $ map (\coord -> getBarrierRow coord coords) row) ++ "|"

drawValueRow :: [(Int, Int, Char)] -> [[(Int, Int, Char)]] -> String
drawValueRow row coords =  "|" ++ (concat $ (map (\coord -> getValueRow coord coords) row)) ++ "|"

getValueRow :: (Int, Int, Char) -> [[(Int, Int, Char)]] -> String
getValueRow (x, y, value) coords
  | x == 8 = "  " ++ [value] ++ "  "
  | getValue (x + 1) y coords /= value = "  " ++ [value] ++ " |"
  | otherwise = "  " ++ [value] ++ "  "

getBarrierRow :: (Int, Int, Char) -> [[(Int, Int, Char)]] -> String
getBarrierRow (x, y, value) coords
  | y == 8 = "-----"
  | x == 8 && getValue x (y + 1) coords == value  = "     "
  | x == 8 && getValue x (y + 1) coords /= value = "-----"
  | getValue x (y + 1) coords /= value = "-----"
  | getValue (x + 1) y coords /= value = "    |"
  | otherwise = "     "

getPoints :: Int -> String -> [(Int, Int, Char)]
getPoints y row = map (\(x, num) -> (x, y, num)) $ zip [0..] row 

