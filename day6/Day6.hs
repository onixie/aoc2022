module Day6 where

import Data.List.Unique (allUnique)

findSOPMarkerPos :: String -> Int
findSOPMarkerPos (x:y:z:rest)
  | rest /= [] = if head rest/=x && head rest/=y && head rest/=z && x/=y && x/=z && y/=z
                 then 4
                 else 1 + findSOPMarkerPos (y:z:rest)
  | otherwise = undefined
findSOPMarkerPos _ = undefined

part1 :: IO ()
part1 = do
  buffer <- readFile "day6/input.txt"
  print $ findSOPMarkerPos buffer

findSOMMarkerPos :: String -> Int
findSOMMarkerPos buffer
  | length buffer < 14 = undefined
  | otherwise =
    if allUnique $ take 14 buffer
    then 14
    else 1+ findSOMMarkerPos (tail buffer)

part2 :: IO ()
part2 = do
  buffer <- readFile "day6/input.txt"
  print $ findSOMMarkerPos buffer
