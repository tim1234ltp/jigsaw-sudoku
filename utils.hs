module Utils
( getPosX
, getPosY
, getPosZone
, getPosValue
, getZone
, getValue
, getSavedState 
) where

getPosY :: (Int, Int, Char, Char) -> Int
getPosY (x, y, zone, value) = y

getPosX :: (Int, Int, Char, Char) -> Int
getPosX (x, y, zone, value) = x

getPosZone :: (Int, Int, Char, Char) -> Char
getPosZone (x, y, zone, value) = zone

getPosValue :: (Int, Int, Char, Char) -> Char
getPosValue (x, y, zone, value) = value

getZone :: Int -> Int -> [[(Int, Int, Char, Char)]] -> Char
getZone x y coords = getPosZone ((coords !! y) !! x)

getValue:: Int -> Int -> [[(Int, Int, Char, Char)]] -> Char
getValue x y coords = getPosZone ((coords !! y) !! x)

getSavedState :: Int -> Int -> [String] -> Char
getSavedState x y states = ((states !! y) !! x)