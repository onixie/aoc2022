{-# LANGUAGE OverloadedStrings #-}

module Day1 where

import Prelude hiding (readFile)
import Data.List (sort)
import Data.Void (Void)
import Data.Text (Text, snoc)
import Data.Text.IO (readFile)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

newtype Calorie = Calorie Int deriving (Show, Num, Ord, Eq)
newtype Elf = Elf [Calorie] deriving (Show, Ord, Eq)

parseCalorie :: Parser Calorie
parseCalorie = Calorie <$> L.decimal <* newline

parseElf :: Parser Elf
parseElf = Elf <$> manyTill parseCalorie newline

allCalorie :: Elf -> Calorie
allCalorie (Elf cs) = sum cs

load :: FilePath -> IO ([Elf], Text)
load file = do
  content <- readFile file
  let parsed = runParser (manyTill parseElf eof) file (snoc content '\n')
  case parsed of
    Left err -> error (errorBundlePretty err)
    Right elves -> return (elves, content)

part1 :: IO ()
part1 = do
  (elves, _) <- load "day1/input.txt"
  print . maximum $ fmap allCalorie elves
  return ()

part2 :: IO ()
part2 = do
  (elves, _) <- load "day1/input.txt"
  print . sum . take 3 . reverse. sort $ fmap allCalorie elves
  return ()
