{-# LANGUAGE OverloadedStrings #-}

module Day2 where

import Prelude hiding (readFile)
import Control.Monad (void)
import Data.Void (Void)
import Data.Text
import Data.Text.IO (readFile)
import Text.Megaparsec
import Text.Megaparsec.Char

class Score a where
  score :: a -> Int

data Play = Rock
          | Paper
          | Scissors
          deriving (Show, Eq)

newtype Round = Round (Play, Play) deriving Show

instance Score Play where
  score Rock = 1
  score Paper = 2
  score Scissors = 3

instance Score Round where
  score (Round (Rock, m@Paper))      = score m + score Win
  score (Round (Paper, m@Scissors))  = score m + score Win
  score (Round (Scissors, m@Rock))   = score m + score Win
  score (Round (n, m)) | n == m      = score m + score Draw
                       | otherwise   = score m + score Lose

data Result = Lose
            | Draw
            | Win
            deriving (Show, Eq)

newtype Round' = Round' (Play, Result) deriving Show

instance Score Result where
  score Lose = 0
  score Draw = 3
  score Win  = 6

instance Score Round' where
  score (Round' (Rock, r@Win))       = score r + score Paper
  score (Round' (Rock, r@Draw))      = score r + score Rock
  score (Round' (Rock, r@Lose))      = score r + score Scissors
  score (Round' (Paper, r@Win))      = score r + score Scissors
  score (Round' (Paper, r@Draw))     = score r + score Paper
  score (Round' (Paper, r@Lose))     = score r + score Rock
  score (Round' (Scissors, r@Win))   = score r + score Rock
  score (Round' (Scissors, r@Draw))  = score r + score Scissors
  score (Round' (Scissors, r@Lose))  = score r + score Paper

type Parser = Parsec Void Text

parseOpponent :: Parser Play
parseOpponent = choice
                [ Rock  <$ char 'A'
                , Paper <$ char 'B'
                , Scissors <$ char 'C'
                ]

parseMe :: Parser Play
parseMe = choice
          [ Rock  <$ char 'X'
          , Paper <$ char 'Y'
          , Scissors <$ char 'Z'
          ]

parseResult :: Parser Result
parseResult = choice
          [ Lose  <$ char 'X'
          , Draw <$ char 'Y'
          , Win <$ char 'Z'
          ]

parseRound :: Parser Round
parseRound = do
  opponent'sPlay <- parseOpponent
  void $ char ' '
  myPlay <- parseMe
  void newline
  return $ Round (opponent'sPlay, myPlay)

parseRound' :: Parser Round'
parseRound' = do
  opponent'sPlay <- parseOpponent
  void $ char ' '
  result <- parseResult
  void newline
  return $ Round' (opponent'sPlay, result)

load :: Score a => Parser a -> FilePath -> IO [a]
load parser file = do
  content <- readFile file
  -- let content = "A Y\nB X\nC Z\n"
  case runParser (manyTill parser eof) file content of
    Left err -> error $ errorBundlePretty err
    Right rounds -> return rounds

part1 :: IO ()
part1 = do
  rounds <- load parseRound "day2/input.txt"
  -- print rounds
  print . sum $ fmap score rounds
  return ()

part2 :: IO ()
part2 = do
  rounds <- load parseRound' "day2/input.txt"
  -- print rounds
  print . sum $ fmap score rounds
  return ()
