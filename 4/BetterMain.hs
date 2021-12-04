module Main where

import Data.IntMap as IntMap (IntMap, fromList, lookup)
import Data.Vector as V (Vector, toList, fromList, (!), zipWith5, (//))

type Grid a = Vector (Vector a)
data Board = Board (IntMap (Int,Int)) (Grid Int) (Grid Int)

main = do
  (header:_:rest) <- fmap lines (readFile "input")

  let ns = map read (splits ',' header)
  let boards = map parseBoard (splits "" rest)

  print (search1 boards ns)
  print (search2 boards ns)

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

transpose :: Grid a -> Grid a
transpose rows = V.zipWith5 f (rows!0) (rows!1) (rows!2) (rows!3) (rows!4) where
  f a b c d e = V.fromList [a,b,c,d,e]

parseBoard :: [String] -> Board
parseBoard [a,b,c,d,e] = Board (makeIndex rows) rows cols where
  rows = V.fromList [f a, f b, f c, f d, f e]
  cols = transpose rows
  f = V.fromList . map read . splits ' '

makeIndex :: Grid Int -> IntMap (Int,Int)
makeIndex rows = IntMap.fromList $ concat $ zipWith f (V.toList rows) [0..4] where
  f row j = zipWith g (V.toList row) [0..4] where
    g n i = (n, (j,i))

modV :: Int -> (a -> a) -> Vector a -> Vector a
modV i f v = let x = v ! i in v // [(i,f x)]

mark :: Int -> Int -> Board -> Board
mark j i (Board ix rows cols) = Board ix rows' cols' where
  rows' = modV j (modV i (+200)) rows
  cols' = modV i (modV j (+200)) cols

markNum :: Int -> Board -> Board
markNum n b@(Board ix _ _) = case findNumber n b of
  Nothing -> b
  Just (j,i) -> mark j i b

findNumber :: Int -> Board -> Maybe (Int,Int)
findNumber num (Board ix _ _) = IntMap.lookup num ix

isWinning :: Board -> Bool
isWinning (Board _ rows cols) = any (all (> 100)) rows || any (all (> 100)) cols

unmarked :: Board -> [Int]
unmarked (Board _ rows _) = concat (map (filter (< 100) . V.toList) (V.toList rows))

dropWinners :: [Board] -> [Board]
dropWinners = filter (not . isWinning)

-- split ' ' " hello  world " = ["","hello","","world",""]
split :: Eq a => a -> [a] -> [[a]]
split s = go where
  go xs = case break (==s) xs of
    (chunk,[])   -> [chunk]
    (chunk,more) -> chunk : go (dropOnly s more)
  dropOnly s y@(x:xs) = if x == s then xs else y

-- splits ' ' " hello  world " = ["hello","world"]
splits :: Eq a => a -> [a] -> [[a]]
splits sep = filter (not . null) . split sep

modAt :: Int -> (a -> a) -> [a] -> [a]
modAt 0 f (x:xs) = f x : xs
modAt i f (x:xs) = x : modAt (i - 1) f xs


