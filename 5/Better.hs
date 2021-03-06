#!cabal
{- cabal:
build-depends: base, vector, vector-algorithms, clock
-}
{-# LANGUAGE TupleSections #-}
module Main where

import Data.Char
import Data.List (group)

import Data.Vector.Storable as V (thaw, freeze, fromList, toList)
import Data.Vector.Storable.Mutable as V (IOVector)
import Data.Vector.Algorithms.Radix as V (sort)

import System.Clock (getTime, diffTimeSpec, toNanoSecs, Clock(..))
import Control.Exception

parseNum :: String -> (Int,String)
parseNum xs =
  let (ss, rest) = span isDigit xs in
  (read ss, rest)

parsePair :: String -> ((Int,Int),String)
parsePair xs =
  let (a,rest) = parseNum xs in
  let (b,rest') = parseNum (drop 1 rest) in
  ((a,b),rest')

parseLine :: String -> Line
parseLine xs =
  let ((a,b), rest) = parsePair xs in
  let ((c,d), _) = parsePair (drop 4 rest) in
  L a b c d

data Line = L Int Int Int Int deriving Show

range :: Int -> Int -> [Int]
range a b | a < b = [a..b]
          | b < a = [a,a-1..b]
          | otherwise = [a]

points :: Line -> [(Int,Int)]
points (L a b c d) | a==c = map (a,) (range b d)
                   | b==d = map (,b) (range a c)
                   | otherwise = zip (range a c) (range b d)

encode :: (Int,Int) -> Int
encode (x,y) = x*1000 + y

encodeLine :: Line -> [Int]
encodeLine l = map encode (points l)

main = do
  t0 <- getTime Monotonic
  v0 <- fmap (V.fromList . concatMap encodeLine . map parseLine . lines) (readFile "input")
  v1 <- V.thaw v0 :: IO (IOVector Int)
  V.sort v1 
  v2 <- V.freeze v1
  let answer = (length . filter (\xs -> length xs > 1) . group . V.toList) v2
  evaluate answer
  t1 <- getTime Monotonic
  print answer
  print (toNanoSecs (diffTimeSpec t1 t0) `divMod` 1000000)
