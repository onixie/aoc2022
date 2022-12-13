{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Strict #-}

module Day11 where

import Prelude hiding (readFile, id, round)
import Control.Monad
import Optics
import Data.Void
import Data.Text hiding (zip, drop, foldl, length, map, take, zipWith, replicate, head, reverse, foldl')
import Data.Text.IO hiding (putStr)
import Data.List
import qualified Data.Map as M
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import GHC.Generics hiding (to)

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "#") (L.skipBlockComment "#-" "-#")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

startingItemsP :: Parser [Integer]
startingItemsP = do
  void $ string "  Starting items: "
  lexeme L.decimal `sepBy` string ", "

type Id = Integer
type Items = [Integer]
data Monkey = Monkey { id::Id, items::Items, operation::Operation, test::Test } deriving (Show, Ord, Eq, Generic)

data Operand = Symbol Text
             | Number Integer
             deriving (Show, Ord, Eq)

data Operation = Add Operand Operand
               | Mul Operand Operand
               | Sub Operand Operand
               | Div Operand Operand
               deriving (Show, Ord, Eq)

data Test = IsDivisibleBy { num :: Integer, monkeyIfTrue :: Int, monkeyIfFalse :: Int }
          deriving (Show, Ord, Eq, Generic)

operandP :: Parser Operand
operandP = choice [Symbol <$> symbol "old", Number <$> lexeme L.decimal]

operatorP :: Parser (Operand -> Operand -> Operation)
operatorP = choice [ Add <$ symbol "+"
                   , Mul <$ symbol "*"
                   , Sub <$ symbol "-"
                   , Div <$ symbol "/"
                   ]

operationP :: Parser Operation
operationP = do
 void $ string "Operation: "
 void $ symbol "new" >> symbol "="
 left  <- operandP
 op    <- operatorP
 right <- operandP
 return $ op left right

testP :: Parser Test
testP = do
  void $ string "Test: divisible by "
  num <- lexeme L.decimal
  void $ string "If true: throw to monkey "
  monT <- lexeme L.decimal
  void $ string "If false: throw to monkey "
  monF <- lexeme L.decimal
  return $ IsDivisibleBy num monT monF

monkeyP :: Parser Monkey
monkeyP = do
  void $ symbol "Monkey"
  id <- lexeme L.decimal <* char ':' <* eol
  items <- startingItemsP
  op <- operationP
  t  <- testP
  return $ Monkey id items op t

load :: FilePath -> IO (M.Map Int Monkey)
load path = do
  content <- readFile path
  case runParser (many monkeyP) path content of
    Left err -> error (errorBundlePretty err)
    Right ms -> return . M.fromList $ zip [0..] ms

operand :: Integer -> Operand -> Integer
operand !i (Symbol "old") = i
operand _ (Symbol _) = undefined
operand _ (Number !n) = n

changeWorryLevel :: Integer -> Operation -> Integer
changeWorryLevel !i (Add l r) = operand i l + operand i r
changeWorryLevel !i (Mul l r) = operand i l * operand i r
changeWorryLevel !i (Sub l r) = operand i l - operand i r
changeWorryLevel !i (Div l r) = operand i l `div` operand i r


inspect :: Int -> (Integer -> Integer) -> M.Map Int Monkey -> M.Map Int Monkey
inspect !src !f !ms = ms & at src % mapped %!~ inspect'
  where
    inspect' m@(Monkey _ (i:is) op _) = m{items=f (changeWorryLevel i op):is}
    inspect' m@(Monkey _ [] _ _) = m

throw :: Int -> M.Map Int Monkey -> M.Map Int Monkey
throw !src !ms =
  case ms ^. at src of
    Just (Monkey _ (i:_) _ (IsDivisibleBy n dst1 dst2)) ->
      throw' i (if i `rem` n == 0 then dst1 else dst2)
    _ -> ms
  where
    throw' !i !dst =
      ms & at dst % mapped % #items %!~ (i:)
         & at src % mapped % #items %!~ drop 1


step1 :: Int -> (Integer -> Integer) -> M.Map Int Monkey -> M.Map Int Monkey
step1 !src !f = throw src . inspect src f

turn1 :: (Integer -> Integer) -> Int -> M.Map Int Monkey -> M.Map Int Monkey
turn1 f src ms = case ms ^.. at src % traversed % #items of
  [is] -> iterate (step1 src f) ms !! length is
  _ -> ms

round1 :: (Integer -> Integer) -> M.Map Int Monkey -> M.Map Int Monkey
round1 f ms =
  foldl' (&) ms (map (turn1 f) [0..M.size ms])

countInspected :: Int -> M.Map Int Monkey -> Integer
countInspected src ms = head (ms ^.. at src % traversed % #items % to (fromIntegral . length))

observe :: (Integer -> Integer) -> Int -> M.Map Int Monkey -> IO (M.Map Int Integer)
observe relief numOfRound ms = round numOfRound ms (M.map (const 0) ms)
  where
    numOfTurn = M.size ms
    round 0 _ totalInspected = do
      --print $ "Round"++show numOfRound
      --mapM_ print ms'
      return totalInspected
    round n ms' totalInspected = do
      --print $ "Round"++show (numOfRound - n)
      --mapM_ print ms'
      (ms'', totalInspected') <- turn 0 ms' totalInspected
      round (n-1) ms'' totalInspected'
    turn n ms' totalInspected
      | n == numOfTurn = return (ms', totalInspected)
      | otherwise = do
          --print $ "Turn"++show n
          --mapM_ print ms'
          turn (n+1) (turn1 relief n ms') (totalInspected & at n % mapped %!~ (+countInspected n ms'))

part1 :: IO ()
part1 = do
  monkeys <- load "day11/input.txt"
  observed <- observe (`div`3) 20 monkeys
  let [m1, m2] = observed ^.. folded & sort & reverse & take 2
  print $ m1 * m2
  return ()

part2 :: IO ()
part2 = do
  monkeys <- load "day11/input.txt"
  let n = monkeys & productOf (folded % #test % #num)
  observed <- observe (`mod`n) 10000 monkeys
  let [m1, m2] = observed ^.. traversed & sort & reverse & take 2
  print $ m1 * m2
  return ()
