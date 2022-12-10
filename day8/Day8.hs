{-# LANGUAGE OverloadedStrings #-}

module Day8 where

import Data.Array
import Data.Maybe
import Data.Text hiding (length, maximum)
import Data.Text.IO
import Prelude hiding (filter, readFile, takeWhile)
import Data.Function ((&))

type Grid = Array (Int, Int) Char

load :: FilePath -> IO Grid
load path = do
  content <- readFile path
  let col = fromMaybe undefined $ findIndex (== '\n') content
      row = count "\n" content
  return $ listArray ((0, 0), (row - 1, col - 1)) . unpack $ filter (/= '\n') content

visible :: Grid -> (Int, Int) -> (Bool, Bool, Bool, Bool)
visible grid (r, c) =
  ( visibleFromNorth 0 r c,
    visibleFromSouth row r c,
    visibleFromWest 0 c r,
    visibleFromEast col c r
  )
  where
    (_, (row, col)) = bounds grid
    visibleFromWest sc tc r'
      | sc < tc = grid ! (r', tc) > grid ! (r', sc) && visibleFromWest (sc + 1) tc r'
      | otherwise = True
    visibleFromEast sc tc r'
      | sc > tc = grid ! (r', tc) > grid ! (r', sc) && visibleFromEast (sc - 1) tc r'
      | otherwise = True
    visibleFromNorth sr tr c'
      | sr < tr = grid ! (tr, c') > grid ! (sr, c') && visibleFromNorth (sr + 1) tr c'
      | otherwise = True
    visibleFromSouth sr tr c'
      | sr > tr = grid ! (tr, c') > grid ! (sr, c') && visibleFromSouth (sr - 1) tr c'
      | otherwise = True

fromAnyDir :: (Bool, Bool, Bool, Bool) -> Bool
fromAnyDir (n, s, w, e) = n || s || w || e

part1 :: IO ()
part1 = do
  grid <- load "day8/input.txt"
  -- print grid
  let (_, (row, col)) = bounds grid
  let isVisible = visible grid
  print $ [(r, c) | r <- [0 .. row], c <- [0 .. col], fromAnyDir $ isVisible (r, c)] & length


sight :: Grid -> (Int, Int) -> (Int, Int, Int, Int)
sight grid (r, c) =
  ( seeNorth (r-1) r c,
    seeSouth (r+1) r c,
    seeWest  (c-1) c r,
    seeEast  (c+1) c r
  )
  where
    (_, (row, col)) = bounds grid
    seeWest sc tc r'
      | sc >= 0 = if grid ! (r', tc) <= grid ! (r', sc) then 1 else 1 + seeWest (sc - 1) tc r'
      | otherwise = 0
    seeEast sc tc r'
      | sc <= col = if grid ! (r', tc) <= grid ! (r', sc) then 1 else 1 + seeEast (sc + 1) tc r'
      | otherwise = 0
    seeNorth sr tr c'
      | sr >= 0 = if grid ! (tr, c') <= grid ! (sr, c') then 1 else 1 + seeNorth (sr - 1) tr c'
      | otherwise = 0
    seeSouth sr tr c'
      | sr <= row = if grid ! (tr, c') <= grid ! (sr, c') then 1 else 1 + seeSouth (sr + 1) tr c'
      | otherwise = 0

scenicScore :: Num a => (a, a, a, a) -> a
scenicScore (n,s,w,e) = n*s*w*e

part2 :: IO ()
part2 = do
  grid <- load "day8/input.txt"
  let (_, (row, col)) = bounds grid
  let viewFrom = sight grid
  print $ maximum [scenicScore $ viewFrom (r, c) | r <- [0 .. row], c <- [0 .. col]]
