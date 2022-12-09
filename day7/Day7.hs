{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase  #-}

module Day7 where

import Prelude hiding (readFile)
import Control.Monad ( void )
import Control.Monad.State ( StateT(runStateT) )
import Data.Char ( isPrint )
import Data.Void ( Void )
import Data.Text ( Text )
import Data.Text.IO (readFile)
import Data.Map ( Map, empty )
import Text.Megaparsec
    ( runParser,
      errorBundlePretty,
      choice,
      many,
      manyTill,
      Parsec,
      MonadParsec(eof, takeWhileP, try, lookAhead) )
import Text.Megaparsec.Char ( char, eol, space1 )
import qualified Text.Megaparsec.Char.Lexer as L
import Optics
    ( NoIx,
      A_Setter,
      Optic,
      Field1(_1),
      Field2(_2),
      folded,
      minimumOf,
      sumOf,
      to,
      (%),
      castOptic,
      (^..),
      mapped,
      use,
      At(at) )
import Optics.State.Operators ((%=), (?=))
import GHC.Generics ( Generic )


newtype File = File { size :: Integer } deriving (Show, Generic)
data Dir = Dir { files :: Map Text File, dirs :: Map Text Dir } deriving (Show, Generic)

type SetDir = Optic A_Setter NoIx Dir Dir (Maybe Dir) (Maybe Dir)
type SetFile = Optic A_Setter NoIx Dir Dir (Maybe File) (Maybe File)
type Context = (Dir, [SetDir])
type Parser = StateT Context (Parsec Void Text)

dir :: Text -> SetDir
dir name = castOptic @A_Setter #dirs % at name

go :: [SetDir] -> SetDir
go [dir1] = dir1
go (dir1:dirs) =
  go dirs % mapped % dir1
go [] = undefined

file :: Text -> SetFile
file name = castOptic @A_Setter #files % at name

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

prompt :: Parser ()
prompt = void $ symbol "$"

parseName :: Parser Text
parseName = takeWhileP Nothing isPrint

commands :: Parser ()
commands = void $ many (choice [try cd, ls])

cd :: Parser ()
cd = do
  void $ prompt *> symbol "cd"
  parseName >>= \case
    "/" -> return ()
    ".." -> _2 %= \case [] -> []
                        _:dirs -> dirs
    name -> _2 %= \dirs -> dir name:dirs
  void eol

ls :: Parser ()
ls = do
  void $ prompt *> symbol "ls"
  void $ manyTill output (lookAhead $ choice [void (char '$'), eof])

output :: Parser ()
output = choice [outDir, outFile]

outDir :: Parser ()
outDir = do
  void $ symbol "dir"
  name <- parseName
  acc <- use _2
  case acc of
    [] -> _1 % dir name ?= Dir empty empty
    dirs -> _1 % go dirs % mapped % dir name ?= Dir empty empty
  void eol

outFile :: Parser ()
outFile = do
  fileSize <- lexeme L.decimal
  name <- parseName
  acc <- use _2
  case acc of
    [] -> _1 % file name ?= File{size=fileSize}
    dirs -> _1 % go dirs % mapped % file name ?= File{size=fileSize}
  void eol

load :: FilePath -> IO Dir
load path = do
  let rootContext = (Dir empty empty, [])
  content <- readFile path
  case runParser (runStateT (commands *> use _1) rootContext) path content of
    Left l -> error $ errorBundlePretty l
    Right (r,_)  -> return r

dirSize :: Dir -> Integer
dirSize (Dir{files,dirs}) =
  let fsize = sumOf (folded % #size) files
      dsize = sumOf (folded % to dirSize) dirs
  in
    fsize + dsize

findDir :: (Dir -> Bool) -> Dir -> [Dir]
findDir predicate root =
  let subdirs = root ^.. #dirs % folded in
    if predicate root
    then root:concatMap (findDir predicate) subdirs
    else concatMap (findDir predicate) subdirs

part1 :: IO ()
part1 = do
  rootDir <- load "day7/input.txt"
  --print rootDir
  let smallDirs = findDir ((<=100000) . dirSize) rootDir
  print $ sumOf (folded % to dirSize) smallDirs

part2 :: IO ()
part2 = do
  rootDir <- load "day7/input.txt"
  let used = dirSize rootDir
      toFree = 30000000 - (70000000 - used)
      satisfied = findDir ((>=toFree) . dirSize) rootDir
  --print used
  --print toFree
  print $ minimumOf (folded % to dirSize) satisfied
