import System.IO
import Control.Monad
import Data.Char
import Load
import Save
import Utils

main = forever $ do
    putStrLn "To start playing, load a game"
    command <- getLine
    executeCommand command

executeCommand :: String -> IO ()
executeCommand cmd
  | cmd == "start" = startSudokuGame cmd
  | otherwise = print "Wrong command."

startSudokuGame :: String -> IO ()
startSudokuGame cmd = do
  fileContents <- readFile "map.txt"
  let allRows = lines fileContents
      mapRows = take (9) allRows
      originalStateRows = drop (9) allRows
      positions = getSudokuState mapRows originalStateRows
      sudokuMap = concat $ getDrawnSudokuMap positions
  putStrLn (unlines sudokuMap)
  makeMove positions

makeMove :: [[(Int, Int, Char, Char)]] -> IO ()
makeMove positions = forever $ do
  putStrLn "Enter the next move in form x y value, e.g. 0 0 2"
  putStrLn "To save the game in its current state use 'save PATH'"
  command <- getLine
  let args = words command
      x = digitToInt $ (args !! 0) !! 0
      y = digitToInt $ (args !! 1) !! 0
      newValue = (args !! 2) !! 0
      newPositions = setSudokuState x y newValue positions
      newSudokuMap = concat $ getDrawnSudokuMap newPositions
  putStrLn (unlines newSudokuMap)
  makeMove newPositions


setSudokuState :: Int -> Int -> Char -> [[(Int, Int, Char, Char)]] -> [[(Int, Int, Char, Char)]] 
setSudokuState x y newValue positions = map (\row -> setStateInRow x y newValue row) positions

setStateInRow :: Int -> Int -> Char -> [(Int, Int, Char, Char)] -> [(Int, Int, Char, Char)]
setStateInRow x y newValue row = map (\position -> returnState x y newValue position) row

returnState :: Int -> Int -> Char -> (Int, Int, Char, Char) -> (Int, Int, Char, Char)
returnState x y newValue position
  | x == getPosX position && y == getPosY position = (x, y, getPosZone position, newValue)
  | otherwise = position