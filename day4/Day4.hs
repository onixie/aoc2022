module Day4 where

import Prelude hiding (readFile)
-- import Control.Monad
import Data.Void
import Data.Text hiding (length, filter)
import Data.Text.IO (readFile)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

newtype Section = Section Int deriving (Show, Ord, Eq)
data Range = Range Section Section deriving (Show)

type Parser = Parsec Void Text

parseSection :: Parser Section
parseSection = Section <$> L.decimal

parseRange :: Parser Range
parseRange = Range <$> (parseSection <* char '-') <*> parseSection

parsePair :: Parser (Range, Range)
parsePair = (,) <$> ( parseRange <* char ',') <*> (parseRange <* newline)

load :: FilePath -> IO [(Range, Range)]
load file = do
  content <- readFile file
  let parsed = runParser (manyTill parsePair eof) file content
  case parsed of
    Left err -> error (errorBundlePretty err)
    Right pairs -> return pairs

fullyContains :: Range -> Range -> Bool
fullyContains (Range sl el) (Range sr er)= sl <= sr && er <= el

contained :: (Range, Range) -> Bool
contained (rl, rr) = rl `fullyContains` rr || rr `fullyContains` rl

part1 :: IO ()
part1 = do
  pairs <- load "day4/input.txt"

  -- forM_ pairs $ \pair -> do
  --   when (contained pair) $
  --     print pair

  print . length $ filter contained pairs

  return ()

overlapped :: (Range, Range) -> Bool
overlapped (Range sl el, Range sr er) = not (el < sr || er < sl)

part2 :: IO ()
part2 = do
  pairs <- load "day4/input.txt"

  -- forM_ pairs $ \pair -> do
  --   when (overlapped pair) $
  --     print pair

  print . length $ filter overlapped pairs

  return ()
