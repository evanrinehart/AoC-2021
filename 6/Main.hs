module Main where

type Table = [(Int,Int)]

main = do
  ns <- fmap (load . map read . split ',') (readFile "input")
  print (count (iterate day ns !! 80))
  print (count (iterate day ns !! 256))

day :: Table -> Table
day t =
  let (-1,spawns):t' = shift t in
  spawn spawns t'

blank   = [(0,0),(1,0),(2,0),(3,0),(4,0),(5,0),(6,0),(7,0),(8,0)]
example = [(0,0),(1,1),(2,1),(3,2),(4,1),(5,0),(6,0),(7,0),(8,0)]

shift [(0,a),(1,b),(2,c),(3,d),(4,e),(5,f),(6,g),(7,h),(8,i)] =
  [(-1,a),(0,b),(1,c),(2,d),(3,e),(4,f),(5,g),(6,h),(7,i),(8,0)]

spawn n [(0,a),(1,b),(2,c),(3,d),(4,e),(5,f),(6,g),(7,h),(8,i)] =
  [(0,a),(1,b),(2,c),(3,d),(4,e),(5,f),(6,g + n),(7,h),(8,i + n)]

count [(0,a),(1,b),(2,c),(3,d),(4,e),(5,f),(6,g),(7,h),(8,i)] =
  a + b + c + d + e + f + g + h + i

modAt :: Int -> (a -> a) -> [a] -> [a]
modAt 0 f (x:xs) = f x : xs
modAt i f (x:xs) = x : modAt (i-1) f xs

load :: [Int] -> Table
load [] = blank
load (n:ns) = modAt n (\(foo,bar) -> (foo,bar+1)) (load ns)

-- split ' ' " hello  world " = ["","hello","","world",""]
split :: Eq a => a -> [a] -> [[a]]
split s = go where
  go xs = case break (==s) xs of
    (chunk,[])   -> [chunk]
    (chunk,more) -> chunk : go (dropOnly s more)
  dropOnly s y@(x:xs) = if x == s then xs else y

