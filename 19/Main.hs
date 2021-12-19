{-# LANGUAGE TupleSections #-}
module Main where

import Prelude hiding (join, insert)

import Data.List as L
import Data.Char
import Data.Ord
import Data.Maybe
import Data.Monoid
import Data.Map (Map)
import Control.Monad hiding (join)
import Control.Monad.State
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import qualified Data.IntMap as IM
import Data.IntMap (IntMap, (!))
import qualified Data.Set as S hiding (split)
import Data.Set (Set)

type Point = (Int,Int,Int)
type Shift = (Int,Int,Int)
type Cube = Set Point
type Rot = Point -> Point


-- a big growing cube, points to begin a test at
-- a glob also tracks locations of scanners (relative to the original scanner)
data Glob = Glob Cube [Point] Int (Set Point)
  deriving Show

data Match = Match Glob Cube Shift Rot

globNew c = Glob c (S.toList c) (S.size c) (S.singleton (0,0,0))

globNextPoint (Glob c (_:ps) n locs) = Glob c ps n locs
globNextPoint (Glob c [] n locs)     = error "out of points"

-- get all diffs between p and points in c
-- try each diff
globTryMerge :: Glob -> Cube -> Maybe Shift
globTryMerge (Glob glob (p:ps) n _) c =
  let ds = map (diff p) (S.toList c)
      test d = if abs (s1 - s2) >= 12 then Just d else Nothing where
        c' = S.map (\q -> add d q) c
        s1 = n + S.size c'
        s2 = S.size (S.union glob c')
  in getFirst $ mconcat $ map (First . test) ds

globTryMerge24 :: Glob -> Cube -> Maybe Match
globTryMerge24 glob c =
  let f rot = fmap (rot,) (globTryMerge glob (S.map rot c))
  in case getFirst $ mconcat $ map (First . f) orientations of
    Just (rot,d) -> Just (Match glob c d rot)
    Nothing      -> Nothing

imTake :: Int -> IntMap a -> (a, IntMap a)
imTake k im = (im ! k, IM.delete k im)

getCube :: Int -> StateT (IntMap Cube) IO Cube
getCube k = state (imTake k)

getKeys :: StateT (IntMap Cube) IO [Int]
getKeys = gets IM.keys

merge :: Match -> Glob
merge (Match (Glob a _ _ locs) c d rot) =
  let a' = S.union a (S.map (add d) (S.map rot c))
  in Glob a' (S.toList a') (S.size a') (S.insert d locs)

search :: IntMap Cube -> IO ()
search scans = flip evalStateT scans $ do
  a <- getCube 2
  g1 <- forceMerge (globNew a) 11
  g2 <- forceMerge g1 20
  g3 <- forceMerge g2 21
  g4 <- forceMerge g3 35
  (Glob _ _ _ locs) <- go g4
  let ds = [dist p q | p <- S.toList locs, q <- S.toList locs]
  lprint (maximum ds)

dist p q = let (x,y,z) = diff p q in abs x + abs y + abs z

boundingBox :: Cube -> ((Int,Int),(Int,Int),(Int,Int))
boundingBox cube = ((x0,x1),(y0,y1),(z0,z1)) where
  x (a,b,c) = a
  y (a,b,c) = b
  z (a,b,c) = c
  xs = S.map x cube
  ys = S.map y cube
  zs = S.map z cube
  x0 = minimum xs
  x1 = maximum xs
  y0 = minimum ys
  y1 = maximum ys
  z0 = minimum zs
  z1 = maximum zs

lprint :: Show a => a -> StateT s IO ()
lprint x = liftIO (putStrLn (show x))

go :: Glob -> StateT (IntMap Cube) IO Glob
go glob = do
  n <- gets IM.size
  if n == 0
    then do
      lprint "out of cubes, we're done?"
      return glob
    else do
      ks <- getKeys
      results <- mapM (tryMerge glob) ks
      case getFirst (mconcat (map First results)) of
        Just (k, glob') -> do
          n <- gets IM.size
          let Glob _ _ m locs = glob'
          lprint ("merged "++show k, "remaining="++show n, "glob size="++show m, "no. scanners=" ++ show (S.size locs))
          modify (IM.delete k)
          go glob'
        Nothing -> do
          let Glob _ ps _ _ = glob
          if L.null ps
            then error "out of points, crashing"
            else go (globNextPoint glob)
  
forceMerge :: Glob -> Int -> StateT (IntMap Cube) IO Glob
forceMerge glob k = do
  c <- getCube k
  case globTryMerge24 glob c of
    Just match -> return (merge match)
    Nothing    -> error "force merge failed"

tryMerge :: Glob -> Int -> StateT (IntMap Cube) IO (Maybe (Int,Glob))
tryMerge glob k = do
  c <- gets (! k)
  case globTryMerge24 glob c of
    Just match -> do
      return (Just (k, merge match))
    Nothing    -> return Nothing




rot0 (x,y,z) = (x,y,z)
rotz1 (x,y,z) = (y,-x,z)
rotz2 = rotz1 . rotz1
rotz3 = rotz1 . rotz2
rotx1 (x,y,z) = (x,-z,y)
rotx2 = rotx1 . rotx1
rotx3 = rotx2 . rotx1
roty1 (x,y,z) = (-z,y,x)
roty2 = roty1 . roty1
roty3 = roty2 . roty1

-- assuming forward is x
allUps :: [Point -> Point]
allUps = [id, rotx1, rotx2, rotx3]

orientations :: [Point -> Point]
orientations = concat
  [L.map (. id) allUps
  ,L.map (. rotz1) allUps
  ,L.map (. rotz2) allUps
  ,L.map (. rotz3) allUps
  ,L.map (. roty1) allUps
  ,L.map (. roty3) allUps]

diff :: Point -> Point -> Shift
diff (a,b,c) (x,y,z) = (a-x, b-y, c-z)

add :: Shift -> Point -> Point
add (a,b,c) (x,y,z) = (a+x,b+y,c+z)

neg (a,b,c) = (-a,-b,-c)

cubeAllWays :: Cube -> [Cube]
cubeAllWays c = L.map (\f -> S.map f c) orientations

testAllWays :: Test -> [Test]
testAllWays (Test h1 c1 h2 c2) = map (\c -> Test h1 c1 h2 c) (cubeAllWays c2)

data Test = Test String Cube String Cube 
  deriving Show

diffTest :: Shift -> Test -> Maybe (Test,Shift)
diffTest sh t@(Test _ c1 _ c2) = if sizeDiff >= 12 then Just (t,sh) else Nothing where
  sizeDiff = abs (m - n)
  m = S.size c1 + S.size c2
  n = S.size (S.union c1 (S.map (add sh) c2))

tryDiffMerge :: Shift -> Cube -> Cube -> Maybe Cube -- *
tryDiffMerge d c1 c2 = if sizeDiff >= 12 then Just c3 else Nothing where
  sizeDiff = abs (m - n)
  m = S.size c1 + S.size c2
  n = S.size c3
  c3 = S.union c1 (S.map (add d) c2)



possibleDiffs :: Cube -> Cube -> [Shift]
possibleDiffs c1 c2 = 
  let (p:_) = S.toList c1 in
  let qs = S.toList c2 in
  let ds = L.map (\q -> diff p q) qs in
  ds ++ L.map neg ds

testCube :: Test -> Maybe (Test,Shift)
testCube t@(Test _ c1 _ c2) =
  getFirst (mconcat (map (First . (\d -> diffTest d t)) (possibleDiffs c1 c2)))

test24 :: Test -> Maybe (Test,Shift)
test24 t = getFirst $ mconcat $ map (First . testCube) (testAllWays t)

compileTestCases :: [(String,Cube)] -> [Test]
compileTestCases scans = [Test h1 c1 h2 c2 | (h1,c1) <- scans, (h2,c2) <- scans, h1 /= h2]

main = search =<< getData

deleteById :: String -> [(String,a)] -> [(String,a)]
deleteById k ((h,c):cs) | k==h = cs
                        | otherwise = (h,c) : deleteById k cs




  
  
unused :: Set Int-> [(Int,a)] -> [(Int,a)]
unused seen xs = filter (\(i,_) -> not (S.member i seen)) xs
  
  

getData = do
  ls <- fmap lines (readFile "input")
  let sections = L.map (\(s:ss) -> (s, L.map parseLine ss)) $ split "" ls
  return (IM.fromList (zip [0..] (map (S.fromList . snd) sections)))
  --return (map (\(h,ss) -> (h,S.fromList ss)) sections)

parseLine :: String -> (Int,Int,Int)
parseLine str =
  let [a,b,c] = split ',' str
  in (read a, read b, read c)

split :: Eq a => a -> [a] -> [[a]]
split s = go where
  go xs = case break (==s) xs of
    (chunk,[])   -> [chunk]
    (chunk,more) -> chunk : go (dropOnly s more)
  dropOnly s y@(x:xs) = if x == s then xs else y

