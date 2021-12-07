module Main where

import Data.List
import Data.Ord

main = do
  ns <- getData
  print ("median", median ns)
  print ("avg", avg ns)
  let hmm = map (\i -> (i,cost1 i ns)) [0..1850]
  let hmm2 = fst (minimumBy (comparing snd) hmm)
  print ("best location part 1", hmm2)
  let hmm3 = map (\i -> (i,cost2 i ns)) [0..1850]
  let hmm4 = fst (minimumBy (comparing snd) hmm3)
  print ("best location part 2", hmm4)
  let costs1 = map (\i -> cost1 i ns) [0..1850]
  let costs2 = map (\i -> cost2 i ns) [0..1850]
  print (minimum costs1)
  print (minimum costs2)

cost1 :: Int -> [Int] -> Int
cost1 target ns = sum (map f ns) where
  f x = abs (target - x)

cost2 :: Int -> [Int] -> Int
cost2 target ns = sum (map f ns) where
  f x = sumo (abs (target - x))

sumo n = n * (n + 1) `div` 2

getData :: IO [Int]
getData = fmap (map read . split ',') (readFile "input")

split :: Eq a => a -> [a] -> [[a]]
split s = go where
  go xs = case break (==s) xs of
    (chunk,[])   -> [chunk]
    (chunk,more) -> chunk : go (dropOnly s more)
  dropOnly s y@(x:xs) = if x == s then xs else y


avg xs = fromIntegral (sum xs) / fromIntegral (length xs)
median xs = (sort xs) !! half where half = length xs `div` 2
