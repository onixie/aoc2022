{-# LANGUAGE OverloadedStrings #-}

module Day2 where

import Prelude hiding (readFile)
import Control.Monad (void)
import Data.Void (Void)
import Data.Text
import Data.Text.IO (readFile)
import Text.Megaparsec
import Text.Megaparsec.Char

data Play = Rock
          | Paper
          | Scissors
          deriving (Show, Eq)

newtype Round = Round (Play, Play) deriving Show

class Score a where
  score :: a -> Int

instance Score Play where
  score Rock = 1
  score Paper = 2
  score Scissors = 3

instance Score Round where
  score (Round (Rock, m@Paper)) = 6 + score m
  score (Round (Paper, m@Scissors)) = 6 + score m
  score (Round (Scissors, m@Rock)) = 6 + score m
  score (Round (n, m)) | n == m = 3 + score m
                       | otherwise = score m

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

parseRound :: Parser Round
parseRound = do
  opponent'sPlay <- parseOpponent
  void $ char ' '
  myPlay <- parseMe
  void newline
  return $ Round (opponent'sPlay, myPlay)

load :: FilePath -> IO [Round]
load file = do
  content <- readFile file
  case runParser (manyTill parseRound eof) file content of
    Left err -> error $ errorBundlePretty err
    Right rounds -> return rounds

part1 :: IO ()
part1 = do
  rounds <- load "day2/input.txt"
  print rounds
  print . sum $ fmap score rounds
  return ()
