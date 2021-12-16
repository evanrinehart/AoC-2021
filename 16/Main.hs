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

type Bit = Int
type Hex = Char

data LenId = BitLength Int | NumPackets Int 
  deriving Show

data Packet = Literal Int Int | Operator Int Int LenId [Packet]
  deriving Show
  

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

sumVersions (Literal v _) = v
sumVersions (Operator v _ _ ps) = v + sum (map sumVersions ps)


operator :: Int -> ([Int] -> Int)
operator 0 = sum
operator 1 = product
operator 2 = minimum
operator 3 = maximum
operator 5 = \[a,b] -> if a > b then 1 else 0
operator 6 = \[a,b] -> if a < b then 1 else 0
operator 7 = \[a,b] -> if a == b then 1 else 0

eval :: Packet -> Int
eval (Literal _ n) = n
eval (Operator _ code _ args) = operator code (map eval args)
    

parsePacket :: [Bit] -> (Packet, [Bit])
parsePacket input = case input of
  (v1:v2:v3:t1:t2:t3:bs) ->
    let version = binaryToInt [v1,v2,v3] in
    let ptype = binaryToInt [t1,t2,t3] in
    case ptype of
      4 -> let (value,rest) = parseLiteral bs in (Literal version value, rest)
      _ -> let (lid,rest) = parseLenId bs in
           let (chunks,rest') = parseChunks lid rest in
           (Operator version ptype lid chunks, rest')

parseLiteral :: [Bit] -> (Int, [Bit])
parseLiteral input = finish (go [] input) where 
  go accum (0:a:b:c:d:rest) = ([a,b,c,d]:accum, rest)
  go accum (1:a:b:c:d:rest) = go ([a,b,c,d]:accum) rest
  finish (nibbles, rest) = (nibblesToInt (reverse nibbles), rest)

parseLenId (0:bs) = (BitLength (binaryToInt (take 15 bs)), drop 15 bs)
parseLenId (1:bs) = (NumPackets (binaryToInt (take 11 bs)), drop 11 bs)

parseChunks :: LenId -> [Bit] -> ([Packet], [Bit])
parseChunks (BitLength n) input = go n [] input where
  go 0 accum bs = (reverse accum, bs)
  go fuel accum bs =
    let (p, bs') = parsePacket bs in
    let used = length bs - length bs' in
    go (fuel - used) (p:accum) bs'
parseChunks (NumPackets n) input = go n [] input where
  go 0 accum bs = (reverse accum, bs)
  go i accum bs = let (p, rest) = parsePacket bs in go (i-1) (p:accum) rest


getData = do
  [line] <- fmap lines (readFile "input")
  return (concat (map hexToBits line))

main = do
  return ()


    

  


split :: Eq a => a -> [a] -> [[a]]
split s = go where
  go xs = case break (==s) xs of
    (chunk,[])   -> [chunk]
    (chunk,more) -> chunk : go (dropOnly s more)
  dropOnly s y@(x:xs) = if x == s then xs else y

