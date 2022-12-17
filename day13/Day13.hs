{-# LANGUAGE OverloadedStrings #-}
module Day13 where

import Control.Monad
import Prelude hiding (readFile)
import Data.Void
import Data.List
import Data.Text
import Data.Text.IO (readFile)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Optics
import Data.Maybe

data Packet = Value Int
            | List [Packet]

type Pair = (Packet, Packet)

instance Show Packet where
  show (Value v) = show v
  show (List ps) = show ps

instance Eq Packet where
  Value vl == Value vr = vl == vr
  List ll == List lr = ll == lr
  Value vl == List [Value vr] = vl == vr
  List [Value vl] == Value vr = vl == vr
  _ == _ = False

instance Ord Packet where
  Value vl `compare` Value vr = vl `compare` vr
  List ll `compare` List lr = ll `compare` lr
  l@(Value _) `compare` r@(List _) = List [l] `compare` r
  l@(List _) `compare` r@(Value _)  = l `compare` List [r]

instance {-# OVERLAPPING #-} Show Pair where
  show (p1,p2) = show p1 ++ "\n" ++ show p2 ++ "\n"

type Parser = Parsec Void Text

pairP :: Parser Pair
pairP = (,) <$> (packetP <* newline) <*> (packetP <* newline)

packetP :: Parser Packet
packetP = do
  choice [Value <$> valueP, List <$> listP]

valueP :: Parser Int
valueP = L.decimal

listP :: Parser [Packet]
listP = char '[' *> packetP `sepBy` char ',' <* char ']'

load :: FilePath -> IO [Pair]
load path = do
  content <- readFile path
  case runParser (pairP `sepEndBy` newline) path content of
    Left err -> error (errorBundlePretty err)
    Right ps -> return ps

isRightOrder :: Pair -> Bool
isRightOrder (p1, p2) = p1 < p2

part1 :: IO ()
part1 = do
  ps <- load "day13/input.txt"
  --mapM_ print ps
  let res = ps & itraversed %~ isRightOrder
  print $ itoListOf (itraversed %& ifiltered (const id)) res & fmap ((+1).fst) & sum

load' :: FilePath -> Text -> IO [Packet]
load' path extra = do
  content <- readFile path
  case runParser (packetP `sepEndBy` many newline) path (content<>extra) of
    Left err -> error (errorBundlePretty err)
    Right ps -> return ps

part2 :: IO ()
part2 = do
  ps <- sort <$> load' "day13/input.txt" "\n[[2]]\n[[6]]\n"
  let d1 = List[List[Value 2]]
  let d2 = List[List[Value 6]]
  -- mapM_ print ps

  let k1 = ifindOf ifolded (const (==d1)) ps & fromJust & fst
  let k2 = ifindOf ifolded (const (==d2)) ps & fromJust & fst
  print $ (k1+1)*(k2+1)
