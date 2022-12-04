module Day3 where

import Prelude hiding (map)
import Data.Functor ((<&>))
import Data.Tuple.Extra (both)
import Data.Set (Set, fromList, intersection, map, toList)
import Data.Char (ord, isLower)

newtype Item = Item Char deriving (Show, Ord, Eq)
newtype Compartment = Compartment [Item] deriving (Show)
newtype Racksack = Racksack (Compartment, Compartment) deriving (Show)

load :: FilePath -> IO [Racksack]
load file = do
  readFile file <&> lines <&> fmap toRacksack
    where
      toRacksack text = Racksack . both Compartment $ splitAt (length text `div` 2) (fmap Item text)

findSameItem :: Racksack -> Set Item
findSameItem (Racksack (Compartment cs1, Compartment cs2)) = fromList cs1 `intersection` fromList cs2

part1 :: IO ()
part1 = do
  racksacks <- load "day3/input.txt"
  print . sum $ concatMap (toList . map priority . findSameItem) racksacks
  return ()

priority :: Item -> Int
priority (Item c) | isLower c = ord c - ord 'a' + 1
                  | otherwise = ord c - ord 'A' + 27
