module Main where

import Prelude hiding (join, insert)

import Data.List
import Data.Char
import Data.Ord
import Data.Maybe
import Data.Map (Map)
import Control.Monad hiding (join)
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import qualified Data.Set as S
import Data.Set (Set)

type Grid = Map (Int,Int) Int
type OpenSet = Set (Int,Int)
type FScore = Map (Int,Int) Int
type GScore = Map (Int,Int) Int
type CameFrom = Map (Int,Int) (Int,Int)

getData = do
  ls <- fmap lines (readFile "input")
  let grid = M.fromList (concat $ zipWith (\j xs -> zipWith (\i x -> ((i,j), digitToInt x)) [0..] xs) [0..] ls)
  
  return grid

h :: (Int,Int) -> Int
h (x,y) = (499 - x) + (499 - y)

main = do
  smallgrid <- getData

  let grid = expandGrid smallgrid
  
  let cameFrom = M.empty
  let gScore = M.singleton (0,0) 0
  let fScore = M.singleton (0,0) (h (0,0))
  let openSet = S.singleton (0,0)

  let path :: Path; path = go grid openSet cameFrom gScore fScore
  let ws :: [Int]; ws = map (weight grid undefined) path

  print (sum ws)

  return ()

getCurrent :: OpenSet -> FScore -> (Int,Int)
getCurrent os fscore = z where
  g xy = case M.lookup xy fscore of
    Nothing -> error "1"
    Just n  -> (xy, n)
  (z,_) = head $ sortBy (comparing snd) (map g (S.toList os))

getNeighbors :: (Int,Int) -> [(Int,Int)]
getNeighbors (x,y) = filter (not . outOfBounds) ps where
  ps = [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]
  outOfBounds (a,b) = a < 0 || b < 0 || a >= 500 || b >= 500

getG :: GScore -> (Int,Int) -> Int
getG gscore xy = case M.lookup xy gscore of
  Nothing -> maxBound
  Just s  -> s

type Path = [(Int,Int)]

reconstructPath :: CameFrom -> (Int,Int) -> Path
reconstructPath cameFrom end = go [] end where
  go path xy = case M.lookup xy cameFrom of
    Nothing  -> path
    Just xy' -> go (xy:path) xy'

go :: Grid -> OpenSet -> CameFrom -> GScore -> FScore -> Path
go grid os cameFrom gscore fscore = if S.null os
  then error "open set is empty"
  else
    let current = getCurrent os fscore in
    if current == (499,499)
      then reconstructPath cameFrom current
      else
        let os' = S.delete current os in
        let neighbors = getNeighbors current in
        go2 grid os' cameFrom gscore fscore current neighbors

weight :: Grid -> (Int,Int) -> (Int,Int) -> Int
weight grid from to = case M.lookup to grid of
  Nothing -> error "2"
  Just w -> w
    

go2 :: Grid -> OpenSet -> CameFrom -> GScore -> FScore -> (Int,Int) -> [(Int,Int)] -> Path
go2 grid os cameFrom gscore fscore current [] = go grid os cameFrom gscore fscore
go2 grid os cameFrom gscore fscore current (n:ns) =
  let tentativeG = getG gscore current + weight grid current n in
  if tentativeG < getG gscore n
    then
      let cameFrom' = M.insert n current cameFrom in
      let gscore' = M.insert n tentativeG gscore in
      let fscore' = M.insert n (tentativeG + h n) fscore in
      let os' = S.insert n os in
      go2 grid os' cameFrom' gscore' fscore' current ns
    else go2 grid os cameFrom gscore fscore current ns
    

genGrid :: Grid -> Int -> Grid
genGrid grid n = M.map (\lvl -> iterate g lvl !! n) grid where
  g x = if x+1 > 9 then 1 else x+1

shiftGrid :: Int -> Int -> Grid -> Grid
shiftGrid x y grid = M.mapKeys (\(a,b) -> (a+x, b+y)) grid


expandGrid :: Grid -> Grid
expandGrid g =
  let indices = [(i,j) | i<-[0..4], j<-[0..4]] in
  let tiles = map (\(i,j) -> shiftGrid (i*100) (j*100) (genGrid g (i+j))) indices in
  foldl1 M.union tiles
  


split :: Eq a => a -> [a] -> [[a]]
split s = go where
  go xs = case break (==s) xs of
    (chunk,[])   -> [chunk]
    (chunk,more) -> chunk : go (dropOnly s more)
  dropOnly s y@(x:xs) = if x == s then xs else y

