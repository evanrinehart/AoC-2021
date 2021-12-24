{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
module House where

import qualified Data.Vector as V
import Data.Vector (Vector, (!), (//))
import Data.List
import qualified Data.Map as M
import Data.Map (Map)
import Data.Foldable

data Piece = PA | PB | PC | PD
data Square = O | A | B | C | D
data Quad a = Q a a a a deriving (Show,Functor)
data HouseF a = H (Vector a) (Quad (Quad a)) deriving (Functor)
type House = HouseF Square
data I2 = I2 Int Int deriving (Eq,Ord,Show)

modVector :: Int -> (a -> a) -> Vector a -> Vector a
modVector i f v = v // [(i, f (v ! i))]

{-
#############
#...........#
###A#C#B#A###
  #D#C#B#A#
  #D#B#A#C#
  #D#D#B#C#
  #########
-}

pzl =
  H (parseHall "...........")
    (Q (Q A C B A)
       (Q D C B A)
       (Q D B A C)
       (Q D D B C))

parseSquare '.' = O
parseSquare 'A' = A
parseSquare 'B' = B
parseSquare 'C' = C
parseSquare 'D' = D

instance Show Square where
  show O = "."
  show A = "A"
  show B = "B"
  show C = "C"
  show D = "D"

instance Show (HouseF Square) where
  show = showHouse

showLevel :: Quad Square -> String
showLevel (Q a b c d) = show a ++ "#" ++ show b ++ "#" ++ show c ++ "#" ++ show d

parseColumn :: String -> Quad Square
parseColumn [a,b,c,d] = Q (f a) (f b) (f c) (f d) where
  f = parseSquare

parseHall :: String -> Vector Square
parseHall = V.fromList . map parseSquare 

showHall :: Vector Square -> String
showHall = concatMap show . V.toList

showHouse :: House -> String
showHouse (H hall (Q a b c d)) = unlines [roof,x,floor,lvl2,lvl3,lvl4,foundation] where
  roof =          "#############"
  x =             "#" ++ showHall hall++"#"
  floor = concat ["###", showLevel a, "###"]
  lvl2  = concat ["  #", showLevel b, "#"]
  lvl3  = concat ["  #", showLevel c, "#"]
  lvl4  = concat ["  #", showLevel d, "#"]
  foundation =    "  #########"

readQuad :: Int -> Quad a -> a
readQuad 0 (Q x _ _ _) = x
readQuad 1 (Q _ x _ _) = x
readQuad 2 (Q _ _ x _) = x
readQuad 3 (Q _ _ _ x) = x

modQuad :: Int -> (a -> a) -> Quad a -> Quad a
modQuad 0 f (Q a b c d) = Q (f a) b c d
modQuad 1 f (Q a b c d) = Q a (f b) c d
modQuad 2 f (Q a b c d) = Q a b (f c) d
modQuad 3 f (Q a b c d) = Q a b c (f d)

readHouse :: I2 -> HouseF a -> a 
readHouse (I2 i 0) (H hall rooms) = hall ! i
readHouse (I2 i j) (H hall rooms) = (readQuad ((i-2) `div` 2) . readQuad (j-1)) rooms

modHouse :: I2 -> (a -> a) -> HouseF a -> HouseF a
modHouse (I2 i 0) f (H hall rooms) = H (modVector i f hall) rooms
modHouse (I2 i j) f (H hall rooms) = H hall (modQuad (j-1) (modQuad c f) rooms) where
  c = (i-2) `div` 2

modHall :: (Vector a -> Vector a) -> HouseF a -> HouseF a
modHall f (H hall rooms) = H (f hall) rooms

modRooms :: (Quad (Quad a) -> Quad (Quad a)) -> HouseF a -> HouseF a
modRooms f (H hall rooms) = H hall (f rooms)

moveAtoB :: I2 -> I2 -> House -> House
moveAtoB x y house =
  let p = readHouse x house
  in (modHouse x (const O) . modHouse y (const p)) house

instance Num I2 where
  I2 a b + I2 c d = I2 (a + c) (b + d)
  negate _ = error "negate"
  (*) = error "*"
  signum = error "signum"
  abs = error "signum"
  fromInteger = error "fromInteger"


links :: I2 -> [I2]
links here@(I2 i j) = look where
  tee x = map (x+) [I2 0 1, I2 1 0, I2 (-1) 0]
  eye x = map (x+) [I2 0 1, I2 0 (-1)]
  dash x = map (x+) [I2 1 0, I2 (-1) 0]
  endl x = [x + I2 1 0]
  endr x = [x + I2 (-1) 0]
  bottom x = [x + I2 0 (-1)]
  look | j==0 && (i==2 || i==4 || i==6 || i==8) = tee here
       | j>0 && j<4 = eye here
       | j==4 = bottom here
       | i==0 = endl here
       | i==10 = endr here
       | otherwise = dash here

walk :: I2 -> I2 -> [I2]
walk x y = delete x (links y)

data Tree a = Tree a [Tree a] deriving (Show,Foldable)
houseTree :: I2 -> HouseF a -> Tree (I2,a)
houseTree start house = go start (links start) where
  go here exits = Tree (here, readHouse here house) (map f exits) where
    f next = go next (walk here next)

measureTree n (Tree (x,_) trees) = Tree (x,n) (map (measureTree (n+1)) trees)

allSpaces :: [I2]
allSpaces = go (I2 0 0) (links (I2 0 0)) where
  go here exits = here : concatMap (\e -> go e (walk here e)) exits

distanceTable = M.fromList (concatMap f allSpaces) where
  f start = map g $ toList (measureTree 0 (houseTree start pzl)) where
    g (y,d) = ((start,y),d)

--traverse :: Applicative f => (a -> f b) -> t a -> f (t b) 

instance Traversable HouseF where
  traverse f house = 
