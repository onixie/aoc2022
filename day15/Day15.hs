{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Day15 where

import Prelude hiding (readFile, take, map)
import Control.Monad
import Data.Void
import Data.Text hiding (concatMap, take, map, length, filter, find)
import Data.Text.IO
import Data.Set hiding (fold, toList, filter)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Optics
import GHC.Generics hiding (to)
import Data.Foldable
import Debug.Trace
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Data.Maybe

type Parser = Parsec Void Text
newtype Sensor = Sensor (Int, Int) deriving (Show, Eq, Ord, Generic)
newtype Beacon = Beacon (Int, Int) deriving (Show, Eq, Ord, Generic)

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

sensorP :: Parser Sensor
sensorP = do
  void $ string "Sensor at x="
  x <- L.signed sc L.decimal
  void $ string ", y="
  y <- L.signed sc L.decimal
  void $ string ": "
  return $ Sensor (x,y)

beaconP :: Parser Beacon
beaconP = do
  void $ string "closest beacon is at x="
  x <- L.signed sc L.decimal
  void $ string ", y="
  y <- L.signed sc L.decimal
  void newline
  return $ Beacon (x,y)

sensorAndBeaconP :: Parser (Sensor, Beacon)
sensorAndBeaconP = (,) <$> sensorP <*> beaconP

load :: FilePath -> IO (Set (Sensor, Beacon))
load path = do
  content <- readFile path
  case runParser (manyTill sensorAndBeaconP eof) path content of
    Left err -> error (errorBundlePretty err)
    Right sb -> return $ fromList sb

manhattan :: (Int,Int) -> (Int,Int) -> Int
manhattan (sx,sy) (bx, by) = abs(by-sy) + abs(bx-sx)

range :: Sensor -> Beacon -> [(Int,Int)]
range (Sensor s) (Beacon b) = range' s
  where
    -- range' :: Set (Int,Int) -> Beacon -> Set (Int,Int)
    -- range' rs beacon@(Beacon b) =
    --   if b `member` (trace (show rs) rs)
    --   then rs
    --   else range' (foldMap surrounding rs) beacon
    -- surrounding (x,y) = fromList [(x,y),(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
    distance = manhattan s b
    range' (x,y) = [[(x+dx,y+dy),(x-dx,y-dy),(x-dx,y+dy),(x+dx,y-dy)]| dx <- [0..distance], let dy = distance - dx]
                   & fold

rangeOn :: Int -> Sensor -> Beacon -> [(Int,Int)]
rangeOn targetY (Sensor s@(sx,sy)) (Beacon b) =
  if dx < 0
  then []
  else [(sx-dx,targetY), (sx+dx,targetY)]
  where
    distance = manhattan s b
    dx = distance - abs(targetY - sy)

drawBeacon :: Beacon -> Picture
drawBeacon (Beacon (x,y)) = rectangleWire 1.0 1.0
                            & color blue
                            & translate (fromIntegral x) (fromIntegral y)

drawSensor :: Sensor -> Picture
drawSensor (Sensor (x,y)) = circle 1.0
                            & color red
                            & translate (fromIntegral x) (fromIntegral y)

drawRange :: [(Int,Int)] -> Picture
drawRange sr = sr ^.. folded % to (over both fromIntegral)
               & line


part1 :: IO ()
part1 = do
  sb <- load "day15/input.txt"
  let targetY = 10
  -- let pb = sb ^.. folded % gplate @Beacon % to drawBeacon
  -- let ps = sb ^.. folded % gplate @Sensor % to drawSensor

  -- let sr = map (uncurry range) sb

  -- let maxX = maximumOf (folded % folded % _1) sr & fromJust & fromIntegral
  -- let minX = minimumOf (folded % folded % _1) sr & fromJust & fromIntegral
  -- let maxY = maximumOf (folded % folded % _2) sr & fromJust & fromIntegral
  -- let minY = minimumOf (folded % folded % _2) sr & fromJust & fromIntegral

  -- let pr = sr ^.. folded % to drawRange
  -- let viewPort = viewPortInit{viewPortTranslate=(-(maxX+minX)/2,(maxY+minY)/2), viewPortScale=10}

  let sr' = map (uncurry (rangeOn targetY)) sb
  let maxX' = maximumOf (folded % folded % filtered ((==targetY).snd) % _1) sr'
  let minX' = minimumOf (folded % folded % filtered ((==targetY).snd) % _1) sr'

  print maxX'
  print minX'
  print ((-) <$> maxX' <*> minX')
  -- animate FullScreen
  --           white
  --           (const $ applyViewPortToPicture viewPort . Scale 1.0 (-1.0) . Pictures $ pb++ps++pr)

mergeRange :: [[(Int,Int)]] -> [[(Int,Int)]]
mergeRange xxx@([(x1,y),(x2,_)]:[(x3,_),(x4,_)]:xs) =
  if x2 + 1 >= x3 then mergeRange ([(x1,y),(max x2 x4,y)]:xs) else xxx
mergeRange xxx = xxx

locateTheBeacon :: [[(Int,Int)]] -> Maybe (Int,Int)
locateTheBeacon ([(x1,y),(x2,_)]:[(x3,_),(x4,_)]:xs) =
  if x2 + 1 >= x3 then locateTheBeacon ([(x1,y),(max x2 x4,y)]:xs) else Just (x2 + 1, y)
locateTheBeacon _= Nothing

part2 :: IO ()
part2 = do
  sb <- load "day15/input.txt"
  let targetY = 4000000
  let scanYs = [0..targetY]
  let srd f y = map (f . uncurry (rangeOn y)) sb & toList
  let merged = fmap (mergeRange . filter (/=[]) . traceShowId . srd id) scanYs

  -- mapM_ print merged
  let rangeNotFullyCovered = find ((>1).length) merged
  let theBeacon@(Just (Just (x,y))) = locateTheBeacon <$> rangeNotFullyCovered
  print theBeacon
  print $ x*targetY + y
