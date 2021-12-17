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

-- target area: x=153..199, y=-114..-75

-- vx min = 17
-- vx max = 199
-- vy min = -114
-- vy max = 300

data Particle = P Int Int Int Int deriving Show
data Target = T Int Int Int Int deriving Show

physics :: Particle -> Particle
physics (P x y vx vy) =
  P (x+vx) (y+vy) (if vx < 0 then vx+1 else if vx > 0 then vx - 1 else 0) (vy - 1)

isInTarget :: Particle -> Target -> Bool
isInTarget (P x y _ _) (T l r b t) =
  l <= x && x <= r && b <= y && y <= t

everInTarget :: Target -> Int -> Int -> Bool
everInTarget t vx vy =
  let ps = takeWhile (notBelow t) $ iterate physics (P 0 0 vx vy) in
  any (\p -> isInTarget p t) ps

notBelow :: Target -> Particle -> Bool
notBelow (T l r b t) (P x y _ _) = not (y < b)

main = do
  t <- getData
  let initials = [(vx,vy) | vx <- [17..199], vy <- [(-114)..300]]
  let answer = length $ filter id $ map (\(vx,vy) -> everInTarget t vx vy) initials
  print answer

getData :: IO Target
getData = do
  line <- readFile "input"
  let [part1, part2] = split ',' line
  let [_,xbits] = split '=' part1
  let [_,ybits] = split '=' part2
  let (x0,rest) = span (/='.') xbits
  let x1 = drop 2 rest
  let [y0, _, rest] = split '.' ybits
  return (T (read x0) (read x1) (read y0) (read rest))



split :: Eq a => a -> [a] -> [[a]]
split s = go where
  go xs = case break (==s) xs of
    (chunk,[])   -> [chunk]
    (chunk,more) -> chunk : go (dropOnly s more)
  dropOnly s y@(x:xs) = if x == s then xs else y

hexToBits :: Hex -> [Bit]
hexToBits h = go 8 (digitToInt h) where
  go 0 _ = []
  go d i = let (q,r) = divMod i d in q : go (d `div` 2) r

binaryToInt :: [Bit] -> Int
binaryToInt bits = go (reverse bits) where
  go [] = 0
  go (0:bs) = 2 * go bs
  go (1:bs) = 2 * go bs + 1

nibblesToInt :: [[Bit]] -> Int
nibblesToInt ns = go (reverse ns) where
  go [] = 0
  go (n:ns) = 16 * go ns + binaryToInt n
