{-# LANGUAGE LambdaCase #-}
module Main where

import Data.List
import Data.Char
import Data.Ord
import Data.Maybe
import Data.Monoid
import Control.Monad
import Control.Concurrent
import qualified Data.Map.Strict as M; import Data.Map.Strict (Map, (!))
import qualified Data.IntMap as IM; import Data.IntMap (IntMap)
import qualified Data.Set as S hiding (split); import Data.Set (Set)
import Debug.Trace

data Slot = O | A | B | C | D deriving (Eq,Ord,Show)
type Room = R Slot Slot Slot Slot deriving (Eq,Ord,Show)
data Layout = L Slot Slot Room Slot Room Slot Room Slot Room Slot Slot deriving (Eq,Ord)

instance Show Layout where
  show = unlines . renderLayout

cost A = 1
cost B = 10
cost C = 100
cost D = 1000

-- 01 2 3 4 56
--   0 2 4 6
--   1 3 5 7

win = L O O (R A A A A) O (R B B B B) O (R C C C C) O (R D D D D) O O
--ex  = L O O (R B A) O (R C D) O (R B C) O (R D A) O O
pzl = L O O (R A D D D) O (R C C B D) O (R B B A B) O (R A A C C) O O

data S = S Int Int Int Int deriving Show

-- an illegal set of moves that gives a lower bound on solutions
lowerBoundMoves = 
  [S 8 1 2 2
  ,S 8 2 6 2
  ,S 6 1 4 2
  ,S 6 2 4 1
  ,S 4 1 6 1
  ,S 4 2 8 2
  ,S 2 2 8 1]

-- lower bound
-- [9,50,50,400,800,8000,9000]
wth = 9 + 800 + 50 + 50 + 400 + 8000 + 9000

-- ^ is greater than... this V

-- manual solution
-- [2,3,3,9,30,40,50,60,500,500,8000,9000]
what = 9 + 60 + 50 + 500 + 500 + 8000 + 30 + 40 + 2 + 9000 + 3 + 3

-- [9,50,50,400,800,8000,9000]
-- [2,3,3,9,30,40,50,60,500,500,8000,9000]

-- [9,50,50,400,800]
-- [2,3,3,9,30,40,50,60,500,500]

-- 9 
-- 9 60 50 500 


lowerBound :: Int -- 18109
lowerBound = sum (map f lowerBoundMoves) where
  st = makeSpaceTable
  f move = moveCost st pzl move

vizMove :: S -> IO ()
vizMove s = do
  print pzl
  print (applyMove s pzl)
  print (moveCost makeSpaceTable pzl s)

solution =
  [S 6 1 1 0
  ,S 6 2 3 0
  ,S 4 1 6 2
  ,S 8 1 9 0
  ,S 8 2 6 1
  ,S 4 2 8 2
  ,S 3 0 4 2
  ,S 1 0 4 1
  ,S 2 1 1 0
  ,S 2 2 8 1
  ,S 1 0 2 2
  ,S 9 0 2 1
  ]

oldSolution = -- 18197
  [S 8 1 0 0
  ,S 6 1 1 0
  ,S 6 2 3 0
  ,S 4 1 6 2
  ,S 8 2 6 1
  ,S 4 2 8 2
  ,S 3 0 4 2
  ,S 1 0 4 1
  ,S 2 1 1 0
  ,S 2 2 8 1
  ,S 1 0 2 2
  ,S 0 0 2 1
  ]

moveCost :: SpaceTable -> Layout -> S -> Int
moveCost st l (S a b c d) =
  let dist = st ! (I2 a b, I2 c d)
  in dist * cost (layoutLook (I2 a b) l)

totalCost :: SpaceTable -> Layout -> [S] -> Int
totalCost st l [] = 0
totalCost st l (s:ss) = moveCost st l s + totalCost st (applyMove s l) ss

tally :: [S] -> IO ()
tally sol = do
  let st = makeSpaceTable
  start <- getData
  let steps = scanl (\(_,l) s -> (moveCost st l s, applyMove s l)) (0,start) sol
  forM_ steps $ \(cost,l) -> do
    print l
    print cost
    getLine
  let total = sum (map fst steps)
  putStrLn ("total cost: " ++ show total)

applyMove :: S -> Layout -> Layout
applyMove (S a b c d) l = moveXtoY (I2 a b) (I2 c d) l

replay :: [S] -> Layout -> [Layout]
replay [] l = [l]
replay ((S a b c d):ms) l = l : replay ms (moveXtoY (I2 a b) (I2 c d) l)



readSlot '.' = O
readSlot 'A' = A
readSlot 'B' = B
readSlot 'C' = C
readSlot 'D' = D

renderLayout :: Layout -> [String]
renderLayout (L a b (R r0 r4 r8 r12) c (R r1 r5 r9 r13) d (R r2 r6 r10 r14) e (R r3 r7 r11 r15) f g) =
  let h O = '.'; h A = 'A'; h B = 'B'; h C = 'C'; h D = 'D' in
  ["#############"
  ,['#',h a, h b, '.', h c, '.', h d, '.', h e, '.', h f, h g, '#']
  ,"###" ++ [h r0] ++ "#" ++ [h r1] ++ "#" ++ [h r2] ++ "#" ++ [h r3] ++ "###"
  ,"  #" ++ [h r4] ++ "#" ++ [h r5] ++ "#" ++ [h r6] ++ "#" ++ [h r7] ++ "#"
  ,"  #" ++ [h r8] ++ "#" ++ [h r9] ++ "#" ++ [h r10] ++ "#" ++ [h r11] ++ "#"
  ,"  #" ++ [h r12] ++ "#" ++ [h r13] ++ "#" ++ [h r14] ++ "#" ++ [h r15] ++ "#"
  ,"  #########"]

pplayout :: Layout -> IO ()
pplayout = putStrLn . unlines . renderLayout

solve :: Layout -> Int
solve l = error "?"

main = do
  return ()

split :: Eq a => a -> [a] -> [[a]]
split delim input = case break (== delim) input of
  (chunk,[])       -> [chunk]
  (chunk,(_:more)) -> chunk : split delim more

pairs :: [a] -> [(a,a)]
pairs zs = [(x,y) | x:xs <- tails zs, y <- xs]


-- layout functions

data I2 = I2 Int Int deriving (Eq,Ord,Show)
neighborSpaces :: I2 -> [I2]
neighborSpaces (I2 0 0) = [I2 1 0]
neighborSpaces (I2 1 0) = [I2 0 0, I2 2 0]
neighborSpaces (I2 2 0) = [I2 1 0, I2 3 0, I2 2 1] 
neighborSpaces (I2 3 0) = [I2 2 0, I2 4 0]
neighborSpaces (I2 4 0) = [I2 3 0, I2 5 0, I2 4 1]
neighborSpaces (I2 5 0) = [I2 4 0, I2 6 0]
neighborSpaces (I2 6 0) = [I2 5 0, I2 7 0, I2 6 1]
neighborSpaces (I2 7 0) = [I2 6 0, I2 8 0]
neighborSpaces (I2 8 0) = [I2 7 0, I2 9 0, I2 8 1]
neighborSpaces (I2 9 0) = [I2 8 0, I2 10 0]
neighborSpaces (I2 10 0) = [I2 9 0]
neighborSpaces (I2 2 1) = [I2 2 0, I2 2 2]
neighborSpaces (I2 4 1) = [I2 4 0, I2 4 2]
neighborSpaces (I2 6 1) = [I2 6 0, I2 6 2]
neighborSpaces (I2 8 1) = [I2 8 0, I2 8 2]
neighborSpaces (I2 2 2) = [I2 2 1]
neighborSpaces (I2 4 2) = [I2 4 1]
neighborSpaces (I2 6 2) = [I2 6 1]
neighborSpaces (I2 8 2) = [I2 8 1]

allSpaces =
  [I2 0 0, I2 1 0, I2 2 0, I2 3 0
  ,I2 4 0, I2 5 0, I2 6 0, I2 7 0
  ,I2 8 0, I2 9 0, I2 10 0
  ,I2 2 1, I2 2 2, I2 4 1, I2 4 2
  ,I2 6 1, I2 6 2, I2 8 1, I2 8 2]

isForbidden (I2 2 0) = True
isForbidden (I2 4 0) = True
isForbidden (I2 6 0) = True
isForbidden (I2 8 0) = True
isForbidden _ = False

spaceMapFrom :: I2 -> [(I2,Int)]
spaceMapFrom start = (start,0) : concatMap (go 1 start) (neighborSpaces start) where
  go d prev here = (here,d) : concatMap (go (d+1) here) (delete prev (neighborSpaces here))

unblocked start layout = concatMap (comeFrom start) (neighborSpaces start) where
  comeFrom prev here = if isForbidden here
    then concatMap (comeFrom here) (delete prev (neighborSpaces here))
    else case layoutLook here layout of
      O -> here : concatMap (comeFrom here) (delete prev (neighborSpaces here))
      _ -> []

validDest :: I2 -> Slot -> Bool
validDest (I2 2 1) p = case p of A -> True; _ -> False
validDest (I2 2 2) p = case p of A -> True; _ -> False
validDest (I2 4 1) p = case p of B -> True; _ -> False
validDest (I2 4 2) p = case p of B -> True; _ -> False
validDest (I2 6 1) p = case p of C -> True; _ -> False
validDest (I2 6 2) p = case p of C -> True; _ -> False
validDest (I2 8 1) p = case p of D -> True; _ -> False
validDest (I2 8 2) p = case p of D -> True; _ -> False
validDest _ _ = True

type SpaceTable = Map (I2,I2) Int
makeSpaceTable :: Map (I2,I2) Int
makeSpaceTable = M.fromList (concatMap f allSpaces) where
  f from = map (\(to,d) -> ((from,to),d)) (spaceMapFrom from)

layoutLook :: I2 -> Layout -> Slot
layoutLook (I2 0 0) (L a b (R r0 r1) c (R r2 r3) d (R r4 r5) e (R r6 r7) f g) = a
layoutLook (I2 1 0) (L a b (R r0 r1) c (R r2 r3) d (R r4 r5) e (R r6 r7) f g) = b
layoutLook (I2 3 0) (L a b (R r0 r1) c (R r2 r3) d (R r4 r5) e (R r6 r7) f g) = c
layoutLook (I2 5 0) (L a b (R r0 r1) c (R r2 r3) d (R r4 r5) e (R r6 r7) f g) = d
layoutLook (I2 7 0) (L a b (R r0 r1) c (R r2 r3) d (R r4 r5) e (R r6 r7) f g) = e
layoutLook (I2 9 0) (L a b (R r0 r1) c (R r2 r3) d (R r4 r5) e (R r6 r7) f g) = f
layoutLook (I2 10 0) (L a b (R r0 r1) c (R r2 r3) d (R r4 r5) e (R r6 r7) f g) = g
layoutLook (I2 2 1) (L a b (R r0 r1) c (R r2 r3) d (R r4 r5) e (R r6 r7) f g) = r0
layoutLook (I2 2 2) (L a b (R r0 r1) c (R r2 r3) d (R r4 r5) e (R r6 r7) f g) = r1
layoutLook (I2 4 1) (L a b (R r0 r1) c (R r2 r3) d (R r4 r5) e (R r6 r7) f g) = r2
layoutLook (I2 4 2) (L a b (R r0 r1) c (R r2 r3) d (R r4 r5) e (R r6 r7) f g) = r3
layoutLook (I2 6 1) (L a b (R r0 r1) c (R r2 r3) d (R r4 r5) e (R r6 r7) f g) = r4
layoutLook (I2 6 2) (L a b (R r0 r1) c (R r2 r3) d (R r4 r5) e (R r6 r7) f g) = r5
layoutLook (I2 8 1) (L a b (R r0 r1) c (R r2 r3) d (R r4 r5) e (R r6 r7) f g) = r6
layoutLook (I2 8 2) (L a b (R r0 r1) c (R r2 r3) d (R r4 r5) e (R r6 r7) f g) = r7

layoutMod h (I2 0 0) (L a b (R r0 r1) c (R r2 r3) d (R r4 r5) e (R r6 r7) f g) =
  (L (h a) b (R r0 r1) c (R r2 r3) d (R r4 r5) e (R r6 r7) f g)

layoutMod h (I2 1 0) (L a b (R r0 r1) c (R r2 r3) d (R r4 r5) e (R r6 r7) f g) =
  (L a (h b) (R r0 r1) c (R r2 r3) d (R r4 r5) e (R r6 r7) f g)

layoutMod h (I2 3 0) (L a b (R r0 r1) c (R r2 r3) d (R r4 r5) e (R r6 r7) f g) =
  (L a b (R r0 r1) (h c) (R r2 r3) d (R r4 r5) e (R r6 r7) f g)

layoutMod h (I2 5 0) (L a b (R r0 r1) c (R r2 r3) d (R r4 r5) e (R r6 r7) f g) =
  (L a b (R r0 r1) c (R r2 r3) (h d) (R r4 r5) e (R r6 r7) f g)

layoutMod h (I2 7 0) (L a b (R r0 r1) c (R r2 r3) d (R r4 r5) e (R r6 r7) f g) =
  (L a b (R r0 r1) c (R r2 r3) d (R r4 r5) (h e) (R r6 r7) f g)

layoutMod h (I2 9 0) (L a b (R r0 r1) c (R r2 r3) d (R r4 r5) e (R r6 r7) f g) =
  (L a b (R r0 r1) c (R r2 r3) d (R r4 r5) e (R r6 r7) (h f) g)

layoutMod h (I2 10 0) (L a b (R r0 r1) c (R r2 r3) d (R r4 r5) e (R r6 r7) f g) =
  (L a b (R r0 r1) c (R r2 r3) d (R r4 r5) e (R r6 r7) f (h g))

layoutMod h (I2 2 1) (L a b (R r0 r1) c (R r2 r3) d (R r4 r5) e (R r6 r7) f g) =
  (L a b (R (h r0) r1) c (R r2 r3) d (R r4 r5) e (R r6 r7) f g)

layoutMod h (I2 2 2) (L a b (R r0 r1) c (R r2 r3) d (R r4 r5) e (R r6 r7) f g) =
  (L a b (R r0 (h r1)) c (R r2 r3) d (R r4 r5) e (R r6 r7) f g)

layoutMod h (I2 4 1) (L a b (R r0 r1) c (R r2 r3) d (R r4 r5) e (R r6 r7) f g) = 
  (L a b (R r0 r1) c (R (h r2) r3) d (R r4 r5) e (R r6 r7) f g)

layoutMod h (I2 4 2) (L a b (R r0 r1) c (R r2 r3) d (R r4 r5) e (R r6 r7) f g) = 
  (L a b (R r0 r1) c (R r2 (h r3)) d (R r4 r5) e (R r6 r7) f g)

layoutMod h (I2 6 1) (L a b (R r0 r1) c (R r2 r3) d (R r4 r5) e (R r6 r7) f g) = 
  (L a b (R r0 r1) c (R r2 r3) d (R (h r4) r5) e (R r6 r7) f g)

layoutMod h (I2 6 2) (L a b (R r0 r1) c (R r2 r3) d (R r4 r5) e (R r6 r7) f g) = 
  (L a b (R r0 r1) c (R r2 r3) d (R r4 (h r5)) e (R r6 r7) f g)

layoutMod h (I2 8 1) (L a b (R r0 r1) c (R r2 r3) d (R r4 r5) e (R r6 r7) f g) = 
  (L a b (R r0 r1) c (R r2 r3) d (R r4 r5) e (R (h r6) r7) f g)

layoutMod h (I2 8 2) (L a b (R r0 r1) c (R r2 r3) d (R r4 r5) e (R r6 r7) f g) = 
  (L a b (R r0 r1) c (R r2 r3) d (R r4 r5) e (R r6 (h r7)) f g)

layoutToList :: Layout -> [(I2,Slot)]
layoutToList (L a b (R r0 r1) c (R r2 r3) d (R r4 r5) e (R r6 r7) f g) =
  [(I2 0 0, a)
  ,(I2 1 0, b)
  ,(I2 3 0, c)
  ,(I2 5 0, d)
  ,(I2 7 0, e)
  ,(I2 9 0, f)
  ,(I2 10 0, g)
  ,(I2 2 1, r0)
  ,(I2 2 2, r1)
  ,(I2 4 1, r2)
  ,(I2 4 2, r3)
  ,(I2 6 1, r4)
  ,(I2 6 2, r5)
  ,(I2 8 1, r6)
  ,(I2 8 2, r7)]

moveXtoY :: I2 -> I2 -> Layout -> Layout
moveXtoY x y layout =
  let p = layoutLook x layout
  in (layoutMod (const O) x . layoutMod (const p) y) layout

isPiece :: Slot -> Bool
isPiece O = False
isPiece _ = True

pieceShouldBeAt :: Slot -> I2
pieceShouldBeAt A = I2 2 1
pieceShouldBeAt B = I2 4 1
pieceShouldBeAt C = I2 6 1
pieceShouldBeAt D = I2 8 1

estimateCost :: SpaceTable -> (I2,Slot) -> Int
estimateCost st (I2 2 2, A) = 0
estimateCost st (I2 4 2, B) = 0
estimateCost st (I2 6 2, C) = 0
estimateCost st (I2 8 2, D) = 0
estimateCost st (x,p) =
  let y = pieceShouldBeAt p
      d = st ! (x,y)
  in d * cost p

pieceLocations :: Layout -> [(I2,Slot)]
pieceLocations = filter (isPiece . snd) . layoutToList

pieceMovesCost :: SpaceTable -> Layout -> I2 -> [(I2,Int)]
pieceMovesCost st layout x =
  let p = layoutLook x layout -- assume piece
      ys = filter (\y -> validDest y p) $ unblocked x layout
  in map (\y -> (y, cost p * (st ! (x,y)))) ys


foo :: SpaceTable -> Layout -> [(Layout,Int)]
foo st layout = concatMap f (pieceLocations layout) where
  f (x,p) = map g (pieceMovesCost st layout x) where
    g (y,w) = (moveXtoY x y layout, w)


-- Astar stuff

type Node = Layout
--type Grid = Map (Int,Int) Int
type OpenSet = Set Node
type FScore = Map Node Int
type GScore = Map Node Int
type CameFrom = Map Node Node
type Path = [Node]
type Neighbors = [Node]

type Move = (Node,Int)

theHFunction :: SpaceTable -> Layout -> Int
theHFunction st = sum . map (estimateCost st) . filter (isPiece . snd) . layoutToList

go :: SpaceTable -> OpenSet -> CameFrom -> GScore -> FScore -> Path
go st os cameFrom gscore fscore = if S.null os
  then error "open set is empty"
  else
    let current = getCurrent os fscore in
    if current == win
      then reconstructPath cameFrom current
      else
        let os' = S.delete current os in
        let neighbors = getNeighbors st current in
        go2 st os' cameFrom gscore fscore current neighbors

go2 :: SpaceTable -> OpenSet -> CameFrom -> GScore -> FScore -> Node -> [Move] -> Path
go2 st os cameFrom gscore fscore current [] = go st os cameFrom gscore fscore
go2 st os cameFrom gscore fscore current ((n,w):ns) =
  let tentativeG = getG gscore current + w in
  if tentativeG < getG gscore n
    then
      let cameFrom' = M.insert n current cameFrom in
      let gscore' = M.insert n tentativeG gscore in
      let fscore' = M.insert n (tentativeG + theHFunction st n) fscore in
      let os' = S.insert n os in
      go2 st os' cameFrom' gscore' fscore' current ns
    else go2 st os cameFrom gscore fscore current ns

reconstructPath :: CameFrom -> Node -> Path
reconstructPath cameFrom end = go [] end where
  go path xy = case M.lookup xy cameFrom of
    Nothing  -> path
    Just xy' -> go (xy:path) xy'

getCurrent :: OpenSet -> FScore -> Node
getCurrent os fscore = z where
  g xy = case M.lookup xy fscore of
    Nothing -> error "1"
    Just n  -> (xy, n)
  (z,_) = head $ sortBy (comparing snd) (map g (S.toList os))

getNeighbors :: SpaceTable -> Node -> [Move]
getNeighbors st layout = foo st layout

getG :: GScore -> Node -> Int
getG gscore xy = case M.lookup xy gscore of
  Nothing -> maxBound
  Just s  -> s
