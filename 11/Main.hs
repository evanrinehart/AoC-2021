module Main where

import Data.List
import Data.Char
import Data.Maybe

type Table a = [[a]]
add2 (a,b) (c,d) = (a+c, b+d)

look :: a -> Int -> Int -> Table a -> a
look d i j tab 
  | i < 0 = d
  | i > 9 = d
  | j < 0 = d
  | j > 9 = d
  | otherwise = tab !! j !! i

data Oct = Oct Int | Energized | Resting
  deriving Show

isEnergized Energized = True
isEnergized _ = False

energize :: Table Int -> Table Oct
energize = mapTable (Oct . (+1))

prepare :: Table Oct -> Table Oct
prepare = mapTable f where
  f (Oct n) = if n > 9 then Energized else Oct n
  f other = other

-- make all energized oct resting and add 1 to adjacents
explode :: Table Oct -> Table Oct
explode tab = zipTable f tab moreEnergy where
  moreEnergy = scanTable (Oct 0) g tab
  g octs = length (filter isEnergized octs)
  f (Oct m) n = Oct (n+m)
  f Energized n = Resting
  f other n = other

stabilize :: Table Oct -> Maybe (Table Int)
stabilize tab = if all good (concat tab) then Just (mapTable f tab) else Nothing
  where
    good Energized = False
    good _ = True
    f (Oct n) = n
    f Resting = 0
    f _ = error "bugs"

turn :: Table Int -> (Int, Table Int)
turn tab = go 0 (prepare (energize tab)) where
  go flashes tab =
    let flashes' = flashes + length (filter isEnergized (concat tab)) in
    let tab' = prepare (explode tab) in
    case stabilize tab' of 
      Nothing -> go flashes' tab'
      Just stable -> (flashes', stable)
      
part1 :: Table Int -> [Int]
part1 tab0 = go tab0 where
  go tab = let (n, tab') = turn tab in n : go tab'


ex :: Table Int
ex =
  [[7,7,7,7,8,3,8,3,5,3]
  ,[2,2,1,7,2,7,2,4,7,8]
  ,[3,3,5,5,3,1,8,6,4,5]
  ,[2,2,4,2,6,1,8,1,1,3]
  ,[7,1,8,2,4,6,8,6,6,6]
  ,[5,4,4,1,6,4,1,1,1,1]
  ,[4,7,7,3,8,6,2,3,6,4]
  ,[5,7,1,7,1,2,5,5,2,1]
  ,[7,5,4,2,1,2,7,7,2,1]
  ,[4,5,7,6,6,7,8,3,4,1]]



mapTable :: (a -> b) -> Table a -> Table b
mapTable f tab = map (map f) tab

genTable :: (Int -> Int -> a) -> Table a
genTable f = map row [0..9] where
  row j = map (\i -> f i j) [0..9]

scanTable :: a -> ([a] -> b) -> Table a -> Table b
scanTable d f tab = genTable g where
  g i j = f $ map (\(x,y) -> look d x y tab) [(i + dx, j + dy)|dx<-[(-1) .. 1], dy<-[(-1) .. 1]]

zipTable :: (a -> b -> c) -> Table a -> Table b -> Table c
zipTable f tab1 tab2 = zipWith (zipWith f) tab1 tab2

main = return ()

getData :: IO (Table Int)
getData = fmap (map (map digitToInt) . lines) (readFile "input")

split :: Eq a => a -> [a] -> [[a]]
split s = go where
  go xs = case break (==s) xs of
    (chunk,[])   -> [chunk]
    (chunk,more) -> chunk : go (dropOnly s more)
  dropOnly s y@(x:xs) = if x == s then xs else y

