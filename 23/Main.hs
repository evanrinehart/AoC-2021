#!cabal
{- cabal:
build-depends: base, vector, containers, mtl
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
module Main where

import qualified Data.Vector as V
import Data.Vector (Vector, (!), (//))
import Data.List
import Data.Maybe
import qualified Data.Map as M
import Data.Map (Map)
import Data.Foldable
import Control.Applicative
import Control.Monad.State
import Debug.Trace

data Piece = PA | PB | PC | PD deriving (Eq,Show)
data Square = O | A | B | C | D deriving (Eq)
data Quad a = Q a a a a deriving (Show,Functor)
data HouseF a = H (Vector a) (Quad (Quad a)) deriving (Functor)
type House = HouseF Square
type Hall = Vector Square
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

modHall :: (Vector a -> Vector a) -> HouseF a -> HouseF a
modHall f (H hall rooms) = H (f hall) rooms

modRooms :: (Quad (Quad a) -> Quad (Quad a)) -> HouseF a -> HouseF a
modRooms f (H hall rooms) = H hall (f rooms)

modColumn :: Piece -> (Quad a -> Quad a) -> HouseF a -> HouseF a
modColumn PA f = modRooms (modQuad 0 f)
modColumn PB f = modRooms (modQuad 1 f)
modColumn PC f = modRooms (modQuad 2 f)
modColumn PD f = modRooms (modQuad 3 f)

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

data Tree a = Tree a [Tree a] deriving (Show,Functor,Foldable)

houseTree :: I2 -> HouseF a -> Tree (I2,a)
houseTree start house = go start (links start) where
  go here exits = Tree (here, readHouse here house) (map f exits) where
    f next = go next (walk here next)

measureTree n (Tree (x,_) trees) = Tree (x,n) (map (measureTree (n+1)) trees)

allSpaces :: [I2]
allSpaces = go (I2 0 0) (links (I2 0 0)) where
  go here exits = here : concatMap (\e -> go e (walk here e)) exits

type DTable = Map (I2,I2) Int

distanceTable :: DTable
distanceTable = M.fromList (concatMap f allSpaces) where
  f start = map g $ toList (measureTree 0 (houseTree start pzl)) where
    g (y,d) = ((start,y),d)

data Column = Mixed | Ready | Complete deriving Show

columnStatus :: Piece -> [Piece] -> Column
columnStatus p ps =
  if all (==p) ps
    then if length ps < 4
      then Ready
      else Complete
    else Mixed

data Move =
  C2F Piece Int | -- column label, free space number (0,1,3,5,7,9,10)
  C2C Piece Piece | -- column, column
  F2C Int Piece -- free space number, destination column
    deriving Show

popCol :: Piece -> Quad [Piece] -> (Piece, Quad [Piece])
popCol PA (Q a b c d) = (head a, Q (tail a) b c d)
popCol PB (Q a b c d) = (head b, Q a (tail b) c d)
popCol PC (Q a b c d) = (head c, Q a b (tail c) d)
popCol PD (Q a b c d) = (head d, Q a b c (tail d))

pushCol :: Piece -> Piece -> Quad [Piece] -> Quad [Piece]
pushCol PA p (Q a b c d) = Q (p : a) b c d
pushCol PB p (Q a b c d) = Q a (p : b) c d
pushCol PC p (Q a b c d) = Q a b (p : c) d
pushCol PD p (Q a b c d) = Q a b c (p : d)


colY0 :: [Piece] -> Int
colY0 [] = 4
colY0 [a] = 3
colY0 [a,b] = 2
colY0 [a,b,c] = 1
colY0 [a,b,c,d] = error "colY0 on full room"

colY1 :: [Piece] -> Int
colY1 [] = error "colY1 on empty room"
colY1 [a] = 4
colY1 [a,b] = 3
colY1 [a,b,c] = 2
colY1 [a,b,c,d] = 1

fp PA = A
fp PB = B
fp PC = C
fp PD = D
tp A = PA
tp B = PB
tp C = PC
tp D = PD


-- function to check for hall blockage

hallClear :: Int -> Int -> Hall -> Bool
hallClear a b hall =
  let c = min a b
      d = max a b
  in V.all (==O) (V.slice c (d-c+1) hall)

putInHall :: Int -> Piece -> Hall -> Hall
putInHall i p hall = hall // [(i,fp p)]

colX :: Piece -> Int
colX PA = 2
colX PB = 4
colX PC = 6
colX PD = 8

freeIs = [0,1,3,5,7,9,10]

free2colClear p i =
  let target = colX p
  in if target < i
    then hallClear target (i-1)
    else hallClear (i+1) target

col2freeClear p i = hallClear (colX p) i

col2colClear p1 p2 = hallClear (colX p1) (colX p2)

genC2CMove :: Piece -> Column -> Piece -> Column -> Hall -> [Move]
genC2CMove c1 Mixed c2 Ready hall = if col2colClear c1 c2 hall then [C2C c1 c2] else []
genC2CMove _ _ _ _ _ = []

genF2CMove :: Piece -> Int -> Column -> Hall -> [Move]
genF2CMove p i Ready hall = if free2colClear p i hall then [F2C i p] else []
genF2CMove p i _     hall = []

colStatusByP :: Piece -> Quad [Piece] -> Column
colStatusByP PA (Q a b c d) = columnStatus PA a
colStatusByP PB (Q a b c d) = columnStatus PB b
colStatusByP PC (Q a b c d) = columnStatus PC c
colStatusByP PD (Q a b c d) = columnStatus PD d


movesF2C :: Quad [Piece] -> Hall -> [Move]
movesF2C cols@(Q a b c d) hall = concatMap f freeIs where
  f i = case hall ! i of
    O -> []
    s -> let p = tp s
             status = colStatusByP p cols
         in genF2CMove p i status hall

movesC2F :: Piece -> [Piece] -> Hall -> [Move]
movesC2F c stack hall = case columnStatus c stack of
  Mixed -> concatMap f freeIs where
    f i = if col2freeClear c i hall then [C2F c i] else []
  _ -> []

movesC2C :: Piece -> [Piece] -> Quad [Piece] -> Hall -> [Move]
movesC2C c stack cols hall = case columnStatus c stack of
  Mixed -> let p = head stack in
           case colStatusByP p cols of
             Ready -> if col2colClear c p hall then [C2C c p] else []
             Complete -> error "movesC2C"
             _ -> []
  _ -> []

pzl' = (Q [PA,PD,PD,PD] [PC,PC,PB,PD] [PB,PB,PA,PB] [PA,PA,PC,PC], parseHall "...........")

moves :: (Quad [Piece], Hall) -> [Move]
moves (cols@(Q a b c d),hall) = concat
  [movesC2F PA a hall
  ,movesC2F PB b hall
  ,movesC2F PC c hall
  ,movesC2F PD d hall
  ,movesC2C PA a cols hall
  ,movesC2C PB b cols hall
  ,movesC2C PC c cols hall
  ,movesC2C PD d cols hall
  ,movesF2C cols hall]


applyMove :: (Quad [Piece],Hall) -> Move -> (Quad [Piece], Hall)
applyMove (cols@(Q a b c d),hall) move = case move of
  C2F c i -> let (p,cols') = popCol c cols in (cols', putInHall i p hall)
  C2C c1 c2 -> let (p,cols') = popCol c1 cols in (pushCol c2 p cols', hall)
  F2C i c -> let p = tp (hall ! i)
                 hall' = hall // [(i,O)]
             in (pushCol c p cols, hall')

won :: S -> Bool
won (Q [PA,PA,PA,PA] [PB,PB,PB,PB] [PC,PC,PC,PC] [PD,PD,PD,PD], _) = True
won _ = False
  
type S = (Quad [Piece], Hall)

stackToQuad [] = Q O O O O
stackToQuad [a] = Q O O O a
stackToQuad [a,b] = Q O O a b
stackToQuad [a,b,c] = Q O a b c
stackToQuad [a,b,c,d] = Q a b c d

padStack [] = [O,O,O,O]
padStack [a] = [O,O,O,fp a]
padStack [a,b] = [O,O,fp a,fp b]
padStack [a,b,c] = [O,fp a,fp b,fp c]
padStack [a,b,c,d] = [fp a, fp b, fp c, fp d]

conv :: S -> House
conv (Q a b c d, hall) =
  let cols = map padStack [a,b,c,d]
      [x,y,z,w] = transpose cols
  in H hall (Q (stackToQuad x) (stackToQuad y) (stackToQuad z) (stackToQuad w))

fun :: S -> IO ()
fun s = case moves s of
  [] -> putStrLn "no more moves"
  (m:_) -> do
    let s' = applyMove s m
    let h = conv s'
    print h
    fun s'

getCol :: Piece -> Quad [Piece] -> [Piece]
getCol PA (Q a b c d) = a
getCol PB (Q a b c d) = b
getCol PC (Q a b c d) = c
getCol PD (Q a b c d) = d

moveDist :: DTable -> S -> Move -> Int
moveDist dt (cols,_) (C2C c1 c2) =
  let p1 = I2 (colX c1) (colY1 (getCol c1 cols))
      p2 = I2 (colX c2) (colY0 (getCol c2 cols))
  in dt M.! (p1,p2)
moveDist dt (cols,_) (C2F c i) =
  let p1 = I2 (colX c) (colY1 (getCol c cols))
      p2 = I2 i 0
  in dt M.! (p1,p2)
moveDist dt (cols,_) (F2C i c) =
  let p1 = I2 i 0
      p2 = I2 (colX c) (colY0 (getCol c cols))
  in dt M.! (p1,p2)

moveCost :: DTable -> S -> Move -> Int
moveCost dt s@(cols,hall) move = pieceCost p * moveDist dt s move where
  p = case move of
    C2F c _ -> head (getCol c cols)
    C2C c1 c2 -> head (getCol c1 cols)
    F2C i _ -> tp (hall ! i)
    

pieceCost PA = 1
pieceCost PB = 10
pieceCost PC = 100
pieceCost PD = 1000

minCost :: Int -> DTable -> S -> Maybe Int
minCost 0 dt s = Nothing
minCost n dt s =
  let f m = fmap (+ moveCost dt s m) (minCost (n-1) dt (applyMove s m))
  in if won s
    then Just 0
    else case catMaybes (map f (moves s)) of
      [] -> Nothing
      costs -> let x = minimum costs in traceShow x (Just x)

main :: IO ()
main = do
  let dt = distanceTable
  let s = pzl'
  let answer = minCost 30 dt s
  print answer
    

