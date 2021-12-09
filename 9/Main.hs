#!cabal
{- cabal:
build-depends: base, vector, containers
-}
module Main where

import Data.Char
import Data.Maybe
import Data.Vector as V (Vector, length, fromList, (!))
import Data.IntSet as IS (IntSet, empty, fromList, size, union, insert, singleton)

import Data.List
import Data.Ord

type Grid = Vector (Vector Int)
type Set = IntSet

main = do
  grid <- loadData
  let lps = getLowPoints grid
  let part1 = sum $ map ((+1) . (\(i,j) -> grid ! j ! i)) lps
  print ("part1", part1)

  let basins :: [Set]; basins = map (getBasin grid) lps
  let sorted :: [Set]; sorted = sortBy (comparing IS.size) basins

  print (map IS.size sorted)
  print (96 * 103 * 106)

  --let basins = map (getBasin grid) lps
  --print basins


highers :: Grid -> (Int,Int) -> [(Int,Int)]
highers grid (i,j) = catMaybes (map f [(i+1,j), (i-1,j), (i,j-1), (i,j+1)]) where
  h = grid ! j ! i
  f (x,y) = let fuck = look grid x y in if fuck < 9 && fuck > h then Just (x,y) else Nothing

single (i,j) = IS.singleton (1000*i + j)

-- start at low point, commit this location
-- for each adjacent location that is higher but less than 9, repeat there
-- stop when there's no higher locations less than 9
getBasin :: Grid -> (Int,Int) -> Set
getBasin grid start = go start where
  go (i,j) =
    let hs = highers grid (i,j) in
    foldl IS.union (single (i,j)) (map go hs)

getLowPoints grid = do
  let width = V.length (grid ! 0)
  let height = V.length grid
  let g (i,j) = ((i,j),isLowPoint grid i j)
  map fst $ filter snd $ map g [(x,y) | x <- [0..width-1], y <- [0..height-1]]

look :: Grid -> Int -> Int -> Int
look v i j | i < 0 = 9
           | j < 0 = 9
           | i >= V.length (v ! 0) = 9
           | j >= V.length v = 9
           | otherwise = v ! j ! i

split :: Eq a => a -> [a] -> [[a]]
split s = go where
  go xs = case break (==s) xs of
    (chunk,[])   -> [chunk]
    (chunk,more) -> chunk : go (dropOnly s more)
  dropOnly s y@(x:xs) = if x == s then xs else y

loadData :: IO Grid
loadData = do
  fmap (V.fromList . map V.fromList . map (map digitToInt) . lines) (readFile "input")

isLowPoint :: Grid -> Int -> Int -> Bool
isLowPoint grid i j =
  let x = look grid i j in
  let n = look grid i (j-1) in
  let s = look grid i (j+1) in
  let e = look grid (i+1) j in
  let w = look grid (i-1) j in
  (x < n) && (x < s) && (x < w) && (x < e)
