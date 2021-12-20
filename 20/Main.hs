{-# LANGUAGE TupleSections #-}
module Main where

import Prelude hiding (join, insert)

import Data.List as L
import Data.Char
import Data.Ord
import Data.Maybe
import Data.Monoid
import Data.Map (Map)
import Control.Monad hiding (join)
import Control.Monad.State
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import qualified Data.IntMap as IM
import Data.IntMap (IntMap, (!))
import qualified Data.Set as S hiding (split)
import Data.Set (Set)

data I2 = I2 !Int !Int deriving (Eq,Ord,Show)
data I3 = I3 !Int !Int !Int deriving (Eq,Ord,Show)
data Img = On (Map I2 ()) | Off (Map I2 ()) deriving Show
type Win = [Int]
type Bounds = ((Int,Int),(Int,Int))


part2 :: String -> Img -> Int
part2 str img =
  let On final = iterate (enhance str) img !! 50 in
  M.size final

enhance :: String -> Img -> Img
enhance str (On m) = genOffImg (boundsPlus (bounds m)) str (On m)
enhance str (Off m) = genOnImg (boundsPlus (bounds m)) str (Off m)

genOnImg :: ((Int,Int),(Int,Int)) -> String -> Img -> Img
genOnImg ((x0,x1),(y0,y1)) str input = On $ M.fromList $ catMaybes (map f [I2 i j | j <- [y0..y1], i <- [x0..x1]]) where
  f ij = let n = decode (window ij input) in case str !! n of '#' -> Just (ij, ()); _ -> Nothing

genOffImg :: ((Int,Int),(Int,Int)) -> String -> Img -> Img
genOffImg ((x0,x1),(y0,y1)) str input = Off $ M.fromList $ catMaybes (map f [I2 i j | j <- [y0..y1], i <- [x0..x1]]) where
  f ij = let n = decode (window ij input) in case str !! n of '.' -> Just (ij, ()); _ -> Nothing

decode :: [Int] -> Int
decode bits = go (reverse bits) where
  go [] = 0
  go (0:bs) = 2 * go bs
  go (1:bs) = 1 + 2 * go bs

readIJ :: I2 -> Img -> Int
readIJ ij (On m) = case M.lookup ij m of
  Just _  -> 1
  Nothing -> 0
readIJ ij (Off m) = case M.lookup ij m of
  Just _  -> 0
  Nothing -> 1

add2 (I2 a b) (I2 c d) = I2 (a+c) (b+d)

window :: I2 -> Img -> Win
window ij img = map f [I2 x y | y <- [(-1)..1], x <- [(-1)..1]] where
  f shift = readIJ (add2 ij shift) img

bounds :: Map I2 () -> ((Int,Int),(Int,Int))
bounds m =
  let ks = M.keys m in
  let xs = map (\(I2 x y) -> x) ks in
  let ys = map (\(I2 x y) -> y) ks in
  ((minimum xs,maximum xs), (minimum ys, maximum ys))

boundsPlus :: Bounds -> Bounds
boundsPlus ((x0,x1),(y0,y1)) = ((x0-1,x1+1),(y0-1,y1+1))

main = do
  (str,img) <- getData
  let ans = part2 str img
  print ans

getData = do
  l1:"":more <- fmap lines (readFile "input")
  return (l1, parseImg more)

parseImg :: [[Char]] -> Img
parseImg rows = On $ M.fromList (catMaybes $ concat $ zipWith g [0..] rows) where
  g j row = zipWith f [0..] row where
    f i '#' = Just (I2 i j, ())
    f i '.' = Nothing

split :: Eq a => a -> [a] -> [[a]]
split s = go where
  go xs = case break (==s) xs of
    (chunk,[])   -> [chunk]
    (chunk,more) -> chunk : go (dropOnly s more)
  dropOnly s y@(x:xs) = if x == s then xs else y

