{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Day5 where

import Prelude hiding (readFile, lines, all, map)
import Control.Monad (void)
import Data.Functor ((<&>))
import Data.Void
import Data.Either (fromRight)
import Data.Text hiding (span, reverse, filter, count, splitAt, foldl)
import Data.Text.IO (readFile)
import Data.Char (isAlphaNum)
import qualified Data.Sequence as S
import Text.Megaparsec hiding (count)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

newtype Crate = Crate Char

instance Show Crate where
  show (Crate c) = show c

newtype Stack a = Stack [a] deriving (Functor)

instance Show a => Show (Stack a) where
  show (Stack cs) = show cs

loadStackOfCrates :: [Text] -> S.Seq (Stack Crate)
loadStackOfCrates drawing = S.fromList . fmap (Stack . fmap Crate . unpack . dropEnd 1) . filter (/="") . filter (all isAlphaNum) . fmap strip $ transpose drawing

data Procedure = Procedure { count :: Int, from :: Int, to :: Int }

instance Show Procedure where
  show (Procedure { count, from, to }) = "move " ++ show count ++ " from " ++ show (from + 1) ++ " to " ++ show (to+1)

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

symbol :: Text -> Parser Text
symbol = L.symbol sc

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

parseProc :: Parser Procedure
parseProc = do
  void $ symbol "move"
  count <- lexeme L.decimal
  void $ symbol "from"
  from <- lexeme L.decimal
  void $ symbol "to"
  to <- lexeme L.decimal
  return Procedure { count, from = from - 1, to = to - 1 }

loadProcedures :: [Text] -> S.Seq Procedure
loadProcedures inputs = S.fromList $ inputs <&> fromRight undefined . runParser parseProc ""

move :: ([Crate] -> [Crate]) -> S.Seq (Stack Crate) -> Procedure  -> S.Seq (Stack Crate)
move craneOp stacks (Procedure{count, from, to}) =
  let (Stack s) = S.index stacks from
      (Stack d) = S.index stacks to
      (m, r) = splitAt count s
  in
    S.update from (Stack r) . S.update to (Stack (craneOp m++d)) $ stacks

part1 :: IO ()
part1 = do
  (drawing, procedures) <- fmap (filter (/="")) . span (/="") . lines <$> readFile "day5/input.txt"

  let stacks = loadStackOfCrates drawing
  let procs  = loadProcedures procedures

  let result = foldl (move reverse) stacks procs
  -- Debug
  -- result2 <- foldM (\ss p -> do
  --                      putStrLn ""
  --                      mapM_ print ss
  --                      print p
  --                      return $ move reverse ss p
  --                  ) stacks procs

  -- mapM_ print result2

  print $ fmap (\case
                   (Stack (Crate c:_)) -> c
                   _ -> undefined
               ) result

part2 :: IO ()
part2 = do
  (drawing, procedures) <- fmap (filter (/="")) . span (/="") . lines <$> readFile "day5/input.txt"

  let stacks = loadStackOfCrates drawing
  let procs  = loadProcedures procedures

  let result = foldl (move id) stacks procs
  -- Debug
  -- result2 <- foldM (\ss p -> do
  --                      putStrLn ""
  --                      mapM_ print ss
  --                      print p
  --                      return $ move id ss p
  --                  ) stacks procs

  -- mapM_ print result2

  print $ fmap (\case
                   (Stack (Crate c:_)) -> c
                   _ -> undefined
               ) result
