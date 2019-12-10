module Utils
( trd
, fth
, getZone
, getValue
, getSavedState 
) where

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