{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE DataKinds #-}

module Day9 where

import Prelude hiding (readFile)
import Optics

import GHC.Generics
import Data.Functor
import Data.Void
import Data.Coerce
import Data.Text hiding (length)
import Data.Text.IO
import Text.Megaparsec hiding (Pos)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Set

type Pos = (Int, Int)
newtype Knot a = Knot Pos deriving (Show, Ord, Eq, Generic)

move :: (Coercible b Pos) => Direction -> b -> b
move (d, m) knot= knot & (coercedTo @Pos) % d %~ m

type Direction = (Lens Pos Pos Int Int, Int -> Int)

up :: Direction
up = (_2, subtract 1)

down :: Direction
down = (_2, (+1))

left :: Direction
left = (_1, subtract 1)

right :: Direction
right = (_1, (+1))

follow :: Knot t -> Knot h-> Knot t
follow follower@(Knot (tx,ty)) (Knot (hx,hy))
  | hx>tx+1&&hy==ty  = follower & move right
  | hx<tx-1&&hy==ty  = follower & move left
  | hx>tx+1&&hy>ty   = follower & move right & move down
  | hx>tx+1&&hy<ty   = follower & move right & move up
  | hx<tx-1&&hy>ty   = follower & move left  & move down
  | hx<tx-1&&hy<ty   = follower & move left  & move up
  | hy>ty+1&&hx==tx  = follower & move down
  | hy<ty-1&&hx==tx  = follower & move up
  | hy>ty+1&&hx>tx   = follower & move down & move right
  | hy>ty+1&&hx<tx   = follower & move down & move left
  | hy<ty-1&&hx>tx   = follower & move up   & move right
  | hy<ty-1&&hx<tx   = follower & move up   & move left
  | otherwise = follower

type Parser = Parsec Void Text

type Motion = (Direction, Int)
motion :: Parser Motion
motion = (,) <$> (dir <* char ' ') <*> steps

dir :: Parser Direction
dir =  choice [char 'U' $> up
              ,char 'D' $> down
              ,char 'L' $> left
              ,char 'R' $> right
              ]

steps :: Parser Int
steps = L.decimal

load :: FilePath -> IO [Motion]
load path = do
  content <- readFile path
  case runParser (sepEndBy motion eol) path content of
    Left err -> error (errorBundlePretty err)
    Right ms -> return ms

simulate :: [Motion] -> Knot h -> Knot t ->  [Knot t]
simulate [] _ t  = [t]
simulate ((_, 0):ms) h t  = simulate ms h t
simulate ((d, s):ms) h t  =
  let h' = h & move d
      t' = t `follow` h'
  in
    t: simulate ((d, s-1):ms) h' t'

part1 :: IO ()
part1 = do
  motions <- load "day9/input.txt"
  print $ simulate motions (Knot (0,0)) (Knot (0,0)) & fromList & length
  return ()

simulate' :: [Motion] -> (Knot h, Knot 1, Knot 2, Knot 3, Knot 4, Knot 5, Knot 6, Knot 7, Knot 8, Knot 9) -> [Knot 9]
simulate' [] (_,_,_,_,_,_,_,_,_,t)  = [t]
simulate' ((_, 0):ms) ks = simulate' ms ks
simulate' ((d, s):ms) (h,k1,k2,k3,k4,k5,k6,k7,k8,k9)  =
  let h' = h & move d
      k1' = k1 `follow` h'
      k2' = k2 `follow` k1'
      k3' = k3 `follow` k2'
      k4' = k4 `follow` k3'
      k5' = k5 `follow` k4'
      k6' = k6 `follow` k5'
      k7' = k7 `follow` k6'
      k8' = k8 `follow` k7'
      k9' = k9 `follow` k8'
  in
    k9: simulate' ((d, s-1):ms) (h',k1',k2',k3',k4',k5',k6',k7',k8',k9')


part2 :: IO ()
part2 = do
  motions <- load "day9/input.txt"
  print $ simulate' motions (Knot (0,0)
                            ,Knot (0,0)
                            ,Knot (0,0)
                            ,Knot (0,0)
                            ,Knot (0,0)
                            ,Knot (0,0)
                            ,Knot (0,0)
                            ,Knot (0,0)
                            ,Knot (0,0)
                            ,Knot (0,0)
                            ) & fromList & length
  return ()
