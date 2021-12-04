module Main where

import Data.List (transpose)
import Data.IntMap as IntMap (IntMap, fromList, lookup)

data Board = Board (IntMap (Int,Int)) [[Int]] [[Int]]

splitBys :: Eq a => a -> [a] -> [[a]]
splitBys sep xs = go (skipSeps xs) where
  skipSeps = dropWhile (==sep)
  skipItems = dropWhile (/=sep)
  grab = takeWhile (/=sep)
  go [] = []
  go xs = (grab xs) : go (skipSeps (skipItems xs))

parseBoard :: [String] -> Board
parseBoard [a,b,c,d,e] = Board (makeIndex rows) rows cols where
  rows = [f a, f b, f c, f d, f e]
  cols = transpose rows
  f = map read . splitBys ' '

makeIndex :: [[Int]] -> IntMap (Int,Int)
makeIndex rows = IntMap.fromList $ concat $ zipWith f rows [0..4] where
  f row j = zipWith g row [0..4] where
    g n i = (n, (j,i))

modAt :: Int -> (a -> a) -> [a] -> [a]
modAt 0 f (x:xs) = f x : xs
modAt i f (x:xs) = x : modAt (i - 1) f xs

mark :: Int -> Int -> Board -> Board
mark j i (Board ix rows cols) = Board ix rows' cols' where
  rows' = modAt j (modAt i (+200)) rows
  cols' = modAt i (modAt j (+200)) cols

markNum :: Int -> Board -> Board
markNum n b@(Board ix _ _) = case findNumber n b of
  Nothing -> b
  Just (j,i) -> mark j i b

findNumber :: Int -> Board -> Maybe (Int,Int)
findNumber num (Board ix _ _) = IntMap.lookup num ix

isWinning :: Board -> Bool
isWinning (Board _ rows cols) = any (all (> 100)) rows || any (all (> 100)) cols

unmarked :: Board -> [Int]
unmarked (Board _ rows _) = concat (map (filter (< 100)) rows)

dropWinners :: [Board] -> [Board]
dropWinners = filter (not . isWinning)

search1 :: [Board] -> [Int] -> Int
search1 boards (n:ns) =
  let boards' = map (markNum n) boards in
  let winners = filter isWinning boards' in
  case winners of
    []  -> search1 boards' ns
    [b] -> sum (unmarked b) * n
    tie -> error "multiple winners!"

search2 :: [Board] -> [Int] -> Int
search2 boards (n:ns) =
  let boards' = map (markNum n) boards in
  let winners = filter isWinning boards' in
  case winners of
    []  -> search2 boards' ns
    [b] ->
      if length boards == 1
        then sum (unmarked b) * n
        else search2 (dropWinners boards') ns
    tie ->
      if length boards == length tie
        then error "multiple last winners!"
        else search2 (dropWinners boards') ns

main = do
  (l0:_:rest) <- fmap lines (readFile "input")

  let ns = map read (splitBys ',' l0) :: [Int]
  let boardsRaw = splitBys "" rest
  let boards = map parseBoard boardsRaw

  print (search1 boards ns)
  print (search2 boards ns)