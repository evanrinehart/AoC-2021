{-# LANGUAGE DeriveFunctor #-}
module Main where

import Control.Monad
import Data.Either
import Data.Maybe
import Data.List


getData = do
  fmap lines (readFile "input")

-- a line is one or more chunks
-- a chunk 'open' followed by zero or more other chunks followed by 'close'

data Error = Incomplete | Corrupted Char deriving Show
newtype Parser a = Parser (String -> Either Error (a, String)) deriving Functor

bail :: Error -> Parser a
bail err = Parser (\_ -> Left err)

parseChar :: Parser Char
parseChar = Parser g where
  g "" = Left Incomplete
  g (c:cs) = Right (c, cs)
  
parseOpen :: Parser Char
parseOpen = do
  c <- parseChar
  case c of
    '[' -> return ']'
    '{' -> return '}'
    '(' -> return ')'
    '<' -> return '>'
    _ -> bail (Corrupted c)

parseClose :: Parser Char
parseClose = do
  c <- parseChar
  case c of
    ']' -> return ']'
    '}' -> return '}'
    ')' -> return ')'
    '>' -> return '>'
    _ -> bail (Corrupted c)

runParser :: Parser a -> String -> Either Error (a,String)
runParser (Parser f) str = case f str of
  Left e -> Left e
  Right (x,str') -> Right (x,str')

newtype Chunk = Chunk [Chunk] deriving Show

parseChunk :: Parser ()
parseChunk = do
  sym1 <- parseOpen
  chunkSequence sym1
  sym2 <- parseClose
  if sym1 == sym2
    then return ()
    else bail (Corrupted '!')

chunkSequence :: Char -> Parser ()
chunkSequence closer = do
  mc <- peek
  case mc of
    Just c  -> if c==closer
      then return ()
      else do
        parseChunk
        chunkSequence closer
    Nothing -> bail Incomplete
  

peek :: Parser (Maybe Char)
peek = Parser g where
  g "" = Right (Nothing, "")
  g (c:cs) = Right (Just c, c:cs)

parseLine = go where
  go = do
    parseChunk
    mc <- peek
    case mc of
      Nothing -> return ()
      Just _  -> go

instance Applicative Parser where
  pure x = Parser (\str -> Right (x,str))
  (<*>) = ap

instance Monad Parser where
  Parser p >>= f = Parser g where
    g str = case p str of
      Left issue -> Left issue
      Right (x,str') -> let Parser q = f x in q str'

isCorrupted (Corrupted c) = Just c
isCorrupted _ = Nothing

score ')' = 3
score ']' = 57
score '}' = 1197
score '>' = 25137

main = do
  ls <- getData
  print (incompletes ls)
  --let results = map (runParser parseLine) ls
  --let bads = catMaybes (map isCorrupted (lefts results))
  --print $ sum (map score bads)

rev '[' = ']'
rev '{' = '}'
rev '(' = ')'
rev '<' = '>'
rev ']' = '['
rev '}' = '{'
rev ')' = '('
rev '>' = '<'

rrev str = map rev (reverse str)

td = "({<<([{{{{({[{<>[]}[{}{}]]{<[]{}><{}{}>}}[<[<>()](<><>)><[{}{}]>])(<{[<>()]<<>()>}<(<>[])>>(([()"

incompletes ls = filter f ls where
  f str = case runParser parseLine str of
    Left Incomplete -> True
    _ -> False

discardChunks :: String -> String
discardChunks "" = ""
discardChunks str = case runParser parseChunk str of
  Right (_, str') -> discardChunks str'
  Left Incomplete -> error "impossible"
  Left (Corrupted c) -> str

autocomplete :: String -> String
autocomplete str = go (rrev str) where
  go trs = case discardChunks trs of
    "" -> ""
    (c : cs) -> c : go cs
    

score2 ')' = 1
score2 ']' = 2
score2 '}' = 3
score2 '>' = 4

scores2 :: String -> Int
scores2 = foldl (\a x -> a * 5 + score2 x) 0


scoring2 :: [String] -> Int
scoring2 ls = median $ map (scores2 . autocomplete) ls


median xs = (sort xs) !! (length xs `div` 2)
