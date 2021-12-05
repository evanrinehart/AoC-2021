#!cabal
{- cabal:
build-depends: base, vector, vector-algorithms
-}
{-# LANGUAGE TupleSections #-}
module Main where

import Data.Char
import Data.List (group)

import Data.Vector.Storable as V (thaw, freeze, fromListN, toList)
import Data.Vector.Storable.Mutable as V (IOVector)
import Data.Vector.Algorithms.AmericanFlag as V (sort)

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

points :: Line -> [(Int,Int)]
points l@(L a b c d) | a==c = map (a,) (range b d)
                     | b==d = map (,b) (range a c)
                     | otherwise = zip (range a c) (range b d)

range :: Int -> Int -> [Int]
range a b | a < b = [a..b]
          | b < a = [a,a-1..b]
          | otherwise = [a]

encode :: (Int,Int) -> Int
encode (x,y) = x*1000 + y

encodeLine :: Line -> [Int]
encodeLine l = map encode (points l)

main = do
  putStrLn "start of program..."
  entries <- fmap (map parseLine . lines) (readFile "input")
  let everything = concatMap encodeLine entries
  v1 <- V.thaw (V.fromListN 1000000 everything) :: IO (IOVector Int)
  V.sort v1 
  v2 <- V.freeze v1
  print $ (length . filter (\xs -> length xs > 1) . group . V.toList) v2
