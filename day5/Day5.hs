{-# LANGUAGE OverloadedStrings #-}
module Day5 where

import Prelude hiding (readFile, lines, all)
import Data.Text hiding (span, reverse, filter)
import Data.Text.IO (readFile)
import Data.List (delete)
import Data.Char (isAlphaNum)

newtype Crate = Crate Char deriving (Show)
newtype Stack = Stack [Crate] deriving (Show)

loadStackOfCrates :: [Text] -> [Text]
loadStackOfCrates drawing = filter (all isAlphaNum) . delete "" . fmap strip $ transpose drawing

data Procedure = Procedure { from :: Int, to :: Int, count :: Int } deriving (Show)

loadProcedures :: [Text] -> [Procedure]
loadProcedures = undefined

part1 :: IO ()
part1 = do
  (drawing, procedures) <- span (/="") . lines <$> readFile "day5/input_test.txt"
  print $ loadStackOfCrates drawing
  print procedures
