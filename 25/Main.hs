#!cabal
{- cabal:
  build-depends: base, vector
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
module Main where

import Data.List
import Data.Char
import Data.Ord
import Data.Maybe
import Data.Monoid
import Control.Monad
import Control.Concurrent
--import qualified Data.Map.Strict as M; import Data.Map.Strict (Map, (!))
--import qualified Data.IntMap as IM; import Data.IntMap (IntMap)
--import qualified Data.Set as S hiding (split); import Data.Set (Set)
import qualified Data.Vector as V
import Data.Vector (Vector, (!))
import Debug.Trace

-- a torus specifically with 137 "rows" and 139 "columns"
data Torus a = Torus (Vector (Vector a)) Int Int deriving (Eq,Show,Functor)

torusH :: Torus a -> Int
torusH (Torus rows _ _) = V.length rows

torusW :: Torus a -> Int
torusW (Torus rows _ _) = V.length (rows ! 0)

readTorus :: Int -> Int -> Torus a -> a
readTorus i j t@(Torus rows di dj) = (rows ! j') ! i' where
  i' = (i + di) `mod` w
  j' = (j + dj) `mod` h
  w = V.length (rows ! 0)
  h = V.length rows

genTorus :: Int -> Int -> (Int -> Int -> a) -> Torus a
genTorus w h g = Torus (V.generate h (\j -> V.generate w (\i -> g i j))) 0 0

duplicateTorus :: Torus a -> Torus (Torus a)
duplicateTorus (Torus rows di dj) = genTorus w h (\i j -> Torus rows (di+i) (dj+j)) where
  w = V.length (rows ! 0)
  h = V.length rows

foldlTorus :: (Int -> Int -> b -> a -> b) -> b -> Torus a -> b
foldlTorus f b0 (Torus rows _ _) = V.ifoldl' g b0 rows where
  g rowb j row = V.ifoldl' (\b i x -> f i j b x) rowb row

type Rules = Fish -> Fish -> Fish -> Fish -> Fish -> Fish -> Fish -> Fish -> Fish -> Fish
data Fish = O | E | V deriving (Eq,Show)

rowToString row = map f (V.toList row) where
  f O = '.'
  f E = '>'
  f V = 'v'

pprint (Torus rows _ _) = mapM_ (\row -> putStrLn (rowToString row)) rows

c2fish '.' = O
c2fish '>' = E
c2fish 'v' = V

val O = 0
val E = 1
val V = 1000

checksum = foldlTorus (\i j b x -> b + val x * j + i) 0

p1 :: Rules
p1 _ _ _ E O _ _ _ _ = E
p1 _ V _ _ O _ _ _ _ = V
p1 _ V _ _ E O _ _ _ = V
p1 _ _ _ _ E O _ _ _ = O
p1 _ _ _ _ V _ _ E O = O
p1 _ _ _ _ V _ O O _ = O
p1 _ _ _ _ V _ V O _ = O
p1 _ _ _ _ x _ _ _ _ = x

step :: Rules -> Torus Fish -> Torus Fish
step rules tbase = fmap g (duplicateTorus tbase) where
  g t = rules
    (readTorus (-1) (-1) t) 
    (readTorus 0 (-1) t) 
    (readTorus 1 (-1) t) 
    (readTorus (-1) 0 t) 
    (readTorus 0 0 t) 
    (readTorus 1 0 t) 
    (readTorus (-1) 1 t) 
    (readTorus 0 1 t) 
    (readTorus 1 1 t) 

ex :: Torus Fish
ex = Torus (V.fromList
  [V.fromList [V,O,O,O]
  ,V.fromList [O,O,O,O]
  ,V.fromList [E,E,E,O]
  ,V.fromList [O,O,V,O]]) 0 0


main :: IO ()
main = do 
  t <- getData
  let final = search 0 t
  print final

search :: Int -> Torus Fish -> Int
search n t = let t' = step p1 t in if t==t' then n+1 else search (n+1) t'

getData = do
  ls <- fmap lines (readFile "input")
  let rows = V.fromList (map (V.fromList . parseLine) ls)
  return (Torus rows 0 0)

parseLine :: String -> [Fish]
parseLine = map c2fish
