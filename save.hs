module Save
( saveMapAndState
) where

getCurrentStateRows :: [[(Int, Int, Char, Char)]] -> [String]
getCurrentStateRows positions = map (\row -> getStateRow row) positions

getStateRow :: [(Int, Int, Char, Char)] -> String
getStateRow row = map (\(x, y, zone, value) -> value) row

saveMapAndState :: String -> [String] -> [[(Int, Int, Char, Char)]] -> IO ()
saveMapAndState path mapRows positions = writeFile path (unlines $ mapRows ++ (getCurrentStateRows positions))