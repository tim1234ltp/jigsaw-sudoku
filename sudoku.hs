import System.IO
import Load
import Save
import Utils

main = do 
    contents <- readFile "map2.txt"
    let allRows = lines contents
        mapRows = take (9) allRows
        originalStateRows = drop (9) allRows
        positions = getSudokuState mapRows originalStateRows
        sudokuMap = concat $ (getDrawnSudokuMap positions)
    putStrLn (unlines sudokuMap)
    saveMapAndState "temp" mapRows positions
