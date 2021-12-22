{-# LANGUAGE LambdaCase #-}
module Main where

import Data.List (tails, nub)

main = do
  cmds <- getData
  --let cs = filter (insideInitArea . snd) cmds
  --mapM_ print cs
  let final = algorithm cmds []
  print final
  print (sizeOfWorld3 final)

algorithm :: [Cmd] -> [I3] -> [I3]
algorithm [] world = world
algorithm ((On, iv):more) world = algorithm more (insert3 iv world)
algorithm ((Off,iv):more) world = algorithm more (delete3 iv world)

getData = do
  fmap (map parseLine . lines) $ readFile "input"

showBlock (I3 (I x0 x1) (I y0 y1) (I z0 z1)) =
  "<" ++ show x0 ++ " " ++ show x1 ++ " " ++ show y0 ++
  " " ++ show y1 ++ " " ++ show z0 ++ " " ++ show z1 ++ ">"

insideInitArea (I3 (I x0 x1) (I y0 y1) (I z0 z1)) =
  x0 >= (-50) &&
  x1 <= 50 &&
  y0 >= (-50) &&
  y1 <= 50 &&
  z0 >= (-50) &&
  z1 <= 50

data I = I !Int !Int deriving (Eq,Show)
data I2 = I2 I I deriving (Eq,Show)
data I3 = I3 I I I deriving (Eq,Show)
type Cmd = (OnOff, I3)
data OnOff = On | Off deriving Show

-- subdivide first interval by the second
subdivide :: I -> I -> [I]
subdivide (I a b) (I c d)
  | b < c || a > d = []
  | c <= a && d >= b = [I a b]
  | c <= a = [I a d, I (d+1) b]
  | d >= b = [I a (c-1), I c b]
  | otherwise = [I a (c-1), I c d, I (d+1) b]

subdivide2 :: I2 -> I2 -> [I2]
subdivide2 (I2 a b) (I2 c d) = [I2 p q | p <- subdivide a c, q <- subdivide b d]

subdivide3 :: I3 -> I3 -> [I3]
subdivide3 (I3 a b c) (I3 d e f) = [I3 p q r | p <- subdivide a d, q <- subdivide b e, r <- subdivide c f]

size :: I -> Int
size (I a b) = b - a + 1

size2 :: I2 -> Int
size2 (I2 a b) = size a * size b

size3 :: I3 -> Int
size3 (I3 a b c) = size a * size b * size c

subset :: I -> I -> Bool
subset (I a b) (I c d) = a >= c && b <= d

subset2 :: I2 -> I2 -> Bool
subset2 (I2 a b) (I2 c d) = a `subset` c && b `subset` d

subset3 :: I3 -> I3 -> Bool
subset3 (I3 a b c) (I3 d e f) = all id [a `subset` d, b `subset` e, c `subset` f]

disjoint (I a b) (I c d) = b < c || a > d
disjoint2 (I2 a b) (I2 c d) = a `disjoint` c || b `disjoint` d
disjoint3 (I3 a b c) (I3 d e f) = any id [a `disjoint` d, b `disjoint` e, c `disjoint` f]

partitionWorld :: I -> [I] -> ([I],[I],[I])
partitionWorld iv world = (inside, complicated, outside) where
  outside = filter (disjoint iv) world
  inside = filter (`subset` iv) world
  complicated = filter (\x -> not (disjoint x iv || subset x iv)) world

partitionWorld2 :: I2 -> [I2] -> ([I2],[I2],[I2])
partitionWorld2 iv world = (inside, complicated, outside) where
  outside = filter (disjoint2 iv) world
  inside = filter (`subset2` iv) world
  complicated = filter (\x -> not (disjoint2 x iv || subset2 x iv)) world

partitionWorld3 :: I3 -> [I3] -> ([I3],[I3],[I3])
partitionWorld3 iv world = (inside, complicated, outside) where
  outside = filter (disjoint3 iv) world
  inside = filter (`subset3` iv) world
  complicated = filter (\x -> not (disjoint3 x iv || subset3 x iv)) world

union :: I -> I -> [I]
union i1 i2
  | disjoint i1 i2 = [i1,i2]
  | i1 `subset` i2 = [i2]
  | i2 `subset` i1 = [i1]
  | otherwise      = nub (subdivide i1 i2 ++ subdivide i2 i1)

isect :: I -> I -> [I]
isect i1 i2
  | disjoint i1 i2 = []
  | i1 `subset` i2 = [i1]
  | i2 `subset` i1 = [i2]
  | otherwise      =
      let [i3,i4] = subdivide i1 i2 in 
      if disjoint i3 i1 then [i4] else [i3]

minus :: I -> I -> [I]
minus i1 i2
  | disjoint i1 i2 = [i1]
  | i1 `subset` i2 = []
  | otherwise = filter (disjoint i2) (subdivide i1 i2)

minus2 :: I2 -> I2 -> [I2]
minus2 i1 i2
  | i1 `disjoint2` i2 = [i1]
  | i1 `subset2` i2 = []
  | otherwise = filter (disjoint2 i2) (subdivide2 i1 i2)

minus3 :: I3 -> I3 -> [I3]
minus3 i1 i2
  | i1 `disjoint3` i2 = [i1]
  | i1 `subset3` i2 = []
  | otherwise = filter (disjoint3 i2) (subdivide3 i1 i2)

subsetOfWorld :: I -> [I] -> Bool
subsetOfWorld iv world = any (iv `subset`) world

subsetOfWorld2 :: I2 -> [I2] -> Bool
subsetOfWorld2 iv world = any (iv `subset2`) world

subsetOfWorld3 :: I3 -> [I3] -> Bool
subsetOfWorld3 iv world = any (iv `subset3`) world

sizeOfWorld :: [I] -> Int
sizeOfWorld = sum . map size

sizeOfWorld2 :: [I2] -> Int
sizeOfWorld2 = sum . map size2

sizeOfWorld3 :: [I3] -> Int
sizeOfWorld3 = sum . map size3

insert :: I -> [I] -> [I]
insert iv world =
  if iv `subsetOfWorld` world
    then world
    else 
      let (inside,complicated,outside) = partitionWorld iv world
          debris = concat (map (`minus` iv) complicated)
      in iv : outside ++ debris

delete :: I -> [I] -> [I]
delete iv world = 
  let (inside,complicated,outside) = partitionWorld iv world
      debris = concat (map (`minus` iv) complicated)
  in outside ++ debris

insert2 :: I2 -> [I2] -> [I2]
insert2 iv world =
  if iv `subsetOfWorld2` world
    then world
    else 
      let (inside,complicated,outside) = partitionWorld2 iv world
          debris = concat (map (`minus2` iv) complicated)
      in iv : outside ++ debris

delete2 :: I2 -> [I2] -> [I2]
delete2 iv world = 
  let (inside,complicated,outside) = partitionWorld2 iv world
      debris = concat (map (`minus2` iv) complicated)
  in outside ++ debris

insert3 :: I3 -> [I3] -> [I3]
insert3 iv world =
  if iv `subsetOfWorld3` world
    then world
    else 
      let (inside,complicated,outside) = partitionWorld3 iv world
          debris = concat (map (`minus3` iv) complicated)
      in iv : outside ++ debris

delete3 :: I3 -> [I3] -> [I3]
delete3 iv world = 
  let (inside,complicated,outside) = partitionWorld3 iv world
      debris = concat (map (`minus3` iv) complicated)
  in outside ++ debris


--box = B (I2 (-50) 50) (I2 (-50) 50) (I2 (-50) 50)

parseLine line =
  let [l,r] = split ' ' line
      [x,y,z] = split ',' r
  in case l of
    "on"   -> (On,  I3 (parseIv x) (parseIv y) (parseIv z))
    "off"  -> (Off, I3 (parseIv x) (parseIv y) (parseIv z))

parseIv str =
  let [_,meat] = split '=' str
      [l,_,r] = split '.' meat
  in I (read l) (read r)

split :: Eq a => a -> [a] -> [[a]]
split delim input = case break (== delim) input of
  (chunk,[])       -> [chunk]
  (chunk,(_:more)) -> chunk : split delim more

pairs :: [a] -> [(a,a)]
pairs zs = [(x,y) | x:xs <- tails zs, y <- xs]

