import System.IO
import Control.Monad
import Data.Char
import Data.List
import Load
import Save
import Utils
import Play

main = forever $ do
    putStrLn "To start playing, use 'start PATH', where PATH points to a saved game."
    command <- getLine
    executeStartCommand command

executeStartCommand :: String -> IO ()
executeStartCommand cmd
  | cmd == "start" = startSudokuGame cmd
  | otherwise = putStrLn "Wrong command"

startSudokuGame :: String -> IO ()
startSudokuGame cmd = do
  fileContents <- readFile "map2.txt"
  let allRows = lines fileContents
      mapRows = take (9) allRows
      originalStateRows = drop (9) allRows
      positions = getSudokuState mapRows originalStateRows
      sudokuMap = concat $ getDrawnSudokuMap positions
  putStrLn (unlines sudokuMap)
  makeMove positions mapRows

makeMove :: [[(Int, Int, Char, Char)]] -> [String] -> IO ()
makeMove positions mapRows = do
  putStrLn "Enter the next move in form x y value, e.g. 0 0 2"
  putStrLn "To save the game in its current state use 'save PATH'"
  command <- getLine
  if isInfixOf "save" command
    then saveMapAndState ((words command) !! 1) mapRows positions
    else do
      let args = words command
          x = digitToInt $ (args !! 0) !! 0
          y = digitToInt $ (args !! 1) !! 0
          newValue = (args !! 2) !! 0
          newPositions = setSudokuState x y newValue positions
          newSudokuMap = concat $ getDrawnSudokuMap newPositions
      if isMoveValid x y newValue newPositions
        then do
          if isGameOver newPositions
            then putStrLn "Congratulations, you finished the game!"
            else do
              putStrLn (unlines newSudokuMap)
              makeMove newPositions mapRows
        else do
          putStrLn "Move was NOT successful, check that the new entry is unique in its column, zone and row."
          makeMove positions mapRows