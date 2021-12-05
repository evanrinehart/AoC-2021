{-# LANGUAGE TupleSections #-}
module Main where

import Data.Char
import Data.Foldable
import Data.Map as M (Map, toList, empty, lookup, insertWith)

parsePair :: String -> ((Int,Int),String)
parsePair xs =
  let (a,rest) = parseNum xs in
  let (b,rest') = parseNum (drop 1 rest) in
  ((a,b),rest')

parseNum :: String -> (Int,String)
parseNum xs =
  let (ss, rest) = span isDigit xs in
  (read ss, rest)

parseLine :: String -> Line
parseLine xs =
  let ((a,b), rest) = parsePair xs in
  let ((c,d), _) = parsePair (drop 4 rest) in
  L a b c d

data Line = L Int Int Int Int deriving Show
type Paper = Map (Int,Int) Int

drawV :: Int -> Int -> Int -> Paper -> Paper
drawV x y1 y2 paper = foldl' h paper (map (x,) (range y1 y2)) where
  h p (a,b) = M.insertWith g (a,b) 1 p
  g u v = u + v

drawH :: Int -> Int -> Int -> Paper -> Paper
drawH y x1 x2 paper = foldl' h paper (map (,y) (range x1 x2)) where
  h p (a,b) = M.insertWith g (a,b) 1 p
  g u v = u + v

drawLine :: Line -> Paper -> Paper
drawLine l@(L a b c d) paper | a==c = drawV a b d paper
                           | b==d = drawH b a c paper
                           | otherwise = error ("bad line = " ++ show l)

range :: Int -> Int -> [Int]
range a b | a < b = [a..b]
          | b < a = [a,a-1..b]
          | otherwise = [a]

straight (L a b c d) = a==c || b==d

main = do
  entries <- fmap (filter straight . map parseLine . lines) (readFile "input")
  let final = foldl' (\p l -> drawLine l p) M.empty entries
  print $ length $ filter (\((x,y),z) -> z > 1) $ M.toList final
  return ()
