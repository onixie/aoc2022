{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Day14 where

import System.Exit
import Control.Monad (void)
import Prelude hiding (readFile)
import Data.Void
import Data.Text hiding (zipWith, tail, concat, concatMap, minimum, maximum, length)
import Data.Text.IO (readFile)
--import Data.Array
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Animate
import Data.List.Extra
import Data.IORef
import Debug.Trace

newtype Scan = Scan [(Int, Int)] deriving Show
data Tile = Air
          | Rock
          | Sand
          deriving Show

type Parser = Parsec Void Text

coordP :: Parser (Int, Int)
coordP = (,) <$> L.decimal <*> (char ',' *> L.decimal)

scanP :: Parser Scan
scanP = Scan <$> coordP `sepBy` string " -> " <* newline

load :: FilePath -> IO [Scan]
load path = do
  content <- readFile path
  case runParser (manyTill scanP eof) path content of
    Left err -> error (errorBundlePretty err)
    Right s  -> return s

scanToCoords :: Scan -> [(Int,Int)]
scanToCoords (Scan sc) =
  concat $ zipWith coords sc (tail sc)
  where
    coords (sx,sy) (ex,ey) =
      [(x,y)| x <- [min sx ex..max sx ex]
            , y <- [min sy ey..max sy ey]]


scanToLines :: Scan -> [Picture]
scanToLines (Scan sc) =
  zipWith path sc (tail sc)
  where
    path (sx,sy) (ex,ey) = Line [(fromIntegral sx, fromIntegral sy),(fromIntegral ex,fromIntegral ey)]

drawRocks :: [Scan] -> Picture
drawRocks scans = Pictures $ concatMap scanToLines scans

drawSands :: [(Int,Int)] -> Picture
drawSands sss = Pictures $ fmap (\(x,y) -> Translate (fromIntegral x) (fromIntegral y) (Circle 0.5)) sss

observeSands :: (Int, Int) -> IORef [(Int,Int)] -> [(Int,Int)]-> IO Picture
observeSands pouringPos sands rocks = do
  readIORef sands >>= \case
    [] -> writeIORef sands [pouringPos]
    s@(x,y):ss ->
      if (x,y+1) `notElem` ss++rocks  then writeIORef sands $ (x,y+1):ss else
        if (x-1,y+1) `notElem` ss++rocks then writeIORef sands $ (x-1,y+1):ss else
          if (x+1,y+1) `notElem` ss++rocks then writeIORef sands $ (x+1,y+1):ss else
            writeIORef sands $ pouringPos:s:ss
  readIORef sands >>= return . drawSands

part1 :: IO ()
part1 = do
  s <- load "day14/input.txt"
  print s
  let allRocks = concatMap scanToCoords s
  let lt = (minimum $ fmap fst allRocks, minimum $ fmap snd allRocks)
  let rb = (maximum $ fmap fst allRocks, maximum $ fmap snd allRocks)
  print lt
  print rb
  let viewPort = viewPortInit {viewPortTranslate=(fromIntegral (0-fst lt), fromIntegral (snd lt)), viewPortScale=5}
  let rocks = drawRocks s
  initSands <- newIORef []
  animateIO FullScreen
            white
            (const $ do
                sands <- observeSands (500,0) initSands allRocks
                return . applyViewPortToPicture viewPort . Scale 1.0 (-1.0) . Pictures $ [sands, rocks])
            (const (return ()))

drawFloor :: Int -> Picture
drawFloor y = Line [(-1e10, fromIntegral y),(1e10, fromIntegral y)]

observeSands' :: (Int, Int) -> IORef [(Int,Int)] -> [(Int,Int)] -> Int -> IO Picture
observeSands' pouringPos sands rocks floorY = do
  readIORef sands >>= \case
    [] -> writeIORef sands [pouringPos]
    sss@((x,y):ss) ->
      if y+1 == floorY then writeIORef sands $ pouringPos:sss else
        if (x,y+1) `notElem` ss++rocks then writeIORef sands $ (x,y+1):ss else
          if (x-1,y+1) `notElem` ss++rocks then writeIORef sands $ (x-1,y+1):ss else
            if (x+1,y+1) `notElem` ss++rocks then writeIORef sands $ (x+1,y+1):ss else
              if (x,y) /= pouringPos then writeIORef sands $ pouringPos:sss else
                print (length sss) >> void exitSuccess
  readIORef sands >>= return . drawSands

calcSands' :: (Int, Int) -> IORef [(Int,Int)] -> [(Int,Int)] -> Int -> IO ()
calcSands' pouringPos sands rocks floorY = do
  readIORef sands >>= \case
    [] -> writeIORef sands [pouringPos]
    sss@((x,y):ss) ->
      if y+1 == floorY then writeIORef sands $ pouringPos:sss else
        if (x,y+1) `notElem` ss++rocks then writeIORef sands $ (x,y+1):ss else
          if (x-1,y+1) `notElem` ss++rocks then writeIORef sands $ (x-1,y+1):ss else
            if (x+1,y+1) `notElem` ss++rocks then writeIORef sands $ (x+1,y+1):ss else
              if (x,y) /= pouringPos then writeIORef sands $ pouringPos:sss else
                print (length sss) >> void exitSuccess
  calcSands' pouringPos sands rocks floorY

part2 :: IO ()
part2 = do
  s <- load "day14/input.txt"
  print s
  let allRocks = concatMap scanToCoords s
  let lt = (minimum $ fmap fst allRocks, minimum $ fmap snd allRocks)
  let rb = (maximum $ fmap fst allRocks, maximum $ fmap snd allRocks)
  let floorY = snd rb + 2
  print lt
  print rb
  -- let viewPort = viewPortInit{viewPortTranslate=(fromIntegral (0-fst lt), fromIntegral (snd lt)), viewPortScale=5}
  -- let rocks = drawRocks s
  -- let caveFloor = drawFloor floorY
  initSands <- newIORef []
  -- animateIO FullScreen
  --           white
  --           (const $ do
  --               sands <- observeSands' (500,0) initSands allRocks floorY
  --               return . applyViewPortToPicture viewPort . Scale 1.0 (-1.0) . Pictures $ [caveFloor, sands, rocks])
  --           (const (return ()))
  calcSands' (500,0) initSands allRocks floorY
