{-# LANGUAGE OverloadedStrings #-}

module Day10 where

import Prelude hiding (readFile, lookup, take, drop)
import Control.Monad
import Data.Function ((&))
import Data.Void
import Data.Text hiding (empty, singleton, take, drop, index, chunksOf)
import Data.Text.IO
import Text.Megaparsec hiding (empty)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Sequence

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "#") (L.skipBlockComment "#-" "-#")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

data Instruction = Addx Int
                 | Noop
                 deriving (Show)

type Program = [Instruction]

programP :: Parser Program
programP = choice [addxP, noopP] `sepEndBy` eol

addxP :: Parser Instruction
addxP = do
  void $ symbol "addx"
  Addx <$> L.signed sc L.decimal

noopP :: Parser Instruction
noopP = do
  void $ string "noop"
  return Noop

load :: FilePath -> IO Program
load path = do
  content <- readFile path
  case runParser programP path content of
    Left err -> error (errorBundlePretty err)
    Right p  -> return p

translate :: Program -> Seq Int -> Seq Int
translate = translate' 1
  where
    translate' x (Noop:ps)   Empty    = singleton x & translate' x ps
    translate' x (Addx v:ps) Empty    = singleton x |> x & translate' (x+v) ps
    translate' x (Noop:ps)   xs       = xs |> x & translate' x ps
    translate' x (Addx v:ps) xs       = xs |> x |> x & translate' (x+v) ps
    translate' x [] ss = ss |> x

part1 :: IO ()
part1 = do
  program <- load "day10/input_test.txt"
  -- print program
  -- print $ translate program empty
  let cycles = fromList [20,60..220]
  let xs = fmap (\c -> translate program empty `index` (c - 1) * c) cycles
  print $ sum xs


drawCRT :: Seq Int -> Seq (Seq Char)
drawCRT = take 6 . chunksOf 40 . mapWithIndex (\ i x -> if i `isDrawnOn` x then '#' else ' ')
  where
    isDrawnOn i x = x - 1 <= i `mod` 40 && i `mod` 40 <= x + 1

part2 :: IO ()
part2 = do
  program <- load "day10/input.txt"
  mapM_ print $ drawCRT (translate program empty)
