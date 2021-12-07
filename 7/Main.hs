module Main where

import Data.Ord
import Data.List

main = do
  ns <- getData
  let costs1 = map (\i -> (i, cost1 i ns)) [0..1850]
  let costs2 = map (\i -> (i, cost2 i ns)) [0..1850]
  print (snd $ minimumBy (comparing snd) costs1)
  print (snd $ minimumBy (comparing snd) costs2)

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
