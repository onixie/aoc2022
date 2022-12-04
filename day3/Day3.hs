module Day3 where

import Prelude hiding (map)
import Data.Functor ((<&>))
import Data.Tuple.Extra (both)
import Data.List.Extra (chunksOf)
import Data.Set (Set, fromList, intersection, union, map, toList)
import Data.Char (ord, isLower)

newtype Item = Item Char deriving (Show, Ord, Eq)
newtype Compartment = Compartment [Item] deriving (Show)
newtype Racksack = Racksack (Compartment, Compartment) deriving (Show)

priority :: Item -> Int
priority (Item c) | isLower c = ord c - ord 'a' + 1
                  | otherwise = ord c - ord 'A' + 27

load :: FilePath -> IO [Racksack]
load file = do
  readFile file <&> lines <&> fmap toRacksack
    where
      toRacksack text = Racksack . both Compartment $ splitAt (length text `div` 2) (fmap Item text)

findSameItem :: Racksack -> Set Item
findSameItem (Racksack (Compartment cs1, Compartment cs2)) = fromList cs1 `intersection` fromList cs2

answer :: (a -> Set Item) -> [a] -> Int
answer f = sum . concatMap (toList . map priority . f)

part1 :: IO ()
part1 = do
  racksacks <- load "day3/input.txt"
  print $ answer findSameItem racksacks
  return ()

findCommonItem :: [Racksack] -> Set Item
findCommonItem = foldr1 intersection . fmap allItem
  where
    allItem (Racksack (Compartment cs1, Compartment cs2)) = fromList cs1 `union` fromList cs2

part2 :: IO ()
part2 = do
  racksacks <- load "day3/input.txt"
  print $ answer findCommonItem (chunksOf 3 racksacks)
  return ()
