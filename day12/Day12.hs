module Day12 where

import Control.Monad (forM)
import Data.Char ( ord )
import Data.Array
import Data.List ( elemIndex, find, sortOn )
import Data.Function
import qualified Data.Set.Ordered as S
-- import Data.Foldable (toList)
import qualified Data.Map as M
import Data.Maybe
-- import Debug.Trace

newtype Map a = Map (Array (Int, Int) a)
type Height = Char

instance Show (Map Char) where
  show (Map arr) = elems arr & to2D
    where
      ((_,_),(_,c)) = bounds arr
      to2D [] = ""
      to2D arrlist = take (c+1) arrlist ++ ['\n'] ++ to2D (drop (c+1) arrlist)

load :: FilePath -> IO (Map Height)
load path = do
  content <- readFile path
  let Just cols = elemIndex '\n' content
  let rows = length $ filter (=='\n') content
  let arr = listArray ((0, 0), (rows-1, cols-1)) (filter (/='\n') content)
  return $ Map arr

print2D :: Map Height -> IO ()
print2D hm = show hm & lines & mapM_ putStrLn

findElevation :: Height -> Map Height -> Maybe (Int, Int)
findElevation s (Map arr) = fmap fst . find (\(_, e) -> e == s) $ assocs arr

dijkstra :: Maybe (Int, Int) -> (Int, Int) -> Map Height -> M.Map (Int,Int) Int -> Map (Maybe (Int, Int)) -> S.OSet (Int,Int) -> S.OSet (Int,Int)
dijkstra Nothing _ _ _ _ _ = S.empty
dijkstra (Just current@(cy,cx)) target (Map m) dist (Map prev) visited =
  if current == target
  then traceBack target
  else dijkstra (shortest dist') target (Map m) dist' (Map prev') (visited S.|> current)
  where
    ((ub,lb),(db,rb)) = bounds m
    neigbors = filter (\neigbor@(ny,nx) ->
                         ub <= ny && ny <= db &&
                         lb <= nx && nx <= rb &&
                         ( m ! current == 'S' && ord (m ! neigbor) <= ord 'b'
                           || m ! neigbor == 'E' && ord (m ! current) >= ord 'y'
                           || (m ! neigbor /= 'E' && ord (m ! neigbor) - ord (m ! current) <= 1 )) &&
                         (dist M.! current + 1) `shorter` (dist M.!? neigbor) &&
                         neigbor `S.notMember` visited
                      )
                      [
                        (cy+1,cx)
                      , (cy-1,cx)
                      , (cy,cx+1)
                      , (cy,cx-1)
                      ]
    dist' = foldl (\mp n -> M.alter (const$ Just (dist M.! current + 1)) n mp) (M.delete current dist) neigbors
    prev' = prev // map (,Just current) neigbors
    shorter _ Nothing = True
    shorter c (Just n) = c <= n
    shortest d = M.toList d
                   & sortOn snd
                   & listToMaybe & fmap fst
    traceBack t = case prev ! t of
      Just p -> traceBack p S.|> t
      _ -> S.empty

part1 :: IO ()
part1 = do
  heightMap@(Map arr) <- load "day12/input.txt"
  -- print2D heightMap
  let Just s = findElevation 'S' heightMap
  let Just e = findElevation 'E' heightMap
  let r = dijkstra (Just s) e heightMap
                   (M.fromList [(s, 0)])
                   (Map (fmap (const Nothing) arr))
                   S.empty
  -- putStrLn ""
  -- print2D . Map $ arr // [(i,' ')| i <- toList r]
  print $ S.size r
  return ()

part2 :: IO ()
part2 = do
  heightMap@(Map arr) <- load "day12/input.txt"
  let ss = assocs arr & filter ((=='a') . snd) & map fst
  let Just e = findElevation 'E' heightMap
  rr <- forM ss $ \s -> do
    let r = dijkstra (Just s) e heightMap
                     (M.fromList [(s, 0)])
                     (Map (fmap (const Nothing) arr))
                     S.empty
    return $ S.size r

  -- print $ rr & filter (/=0)
  print $ rr & filter (/=0) & minimum
  return ()
