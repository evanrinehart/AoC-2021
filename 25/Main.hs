#!cabal
{- cabal:
  build-depends: base, vector
-}
{-# LANGUAGE DeriveFunctor #-}
module Main where

import qualified Data.Vector as V
import Data.Vector (Vector, (!))

cuke :: Rules
cuke _ _ _ E O _ _ _ _ = E
cuke _ V _ _ O _ _ _ _ = V
cuke _ V _ _ E O _ _ _ = V
cuke _ _ _ _ E O _ _ _ = O
cuke _ _ _ _ V _ _ E O = O
cuke _ _ _ _ V _ O O _ = O
cuke _ _ _ _ V _ V O _ = O
cuke _ _ _ _ x _ _ _ _ = x

data Fish = O | E | V deriving Eq

type Rules = Fish -> Fish -> Fish -> Fish -> Fish -> Fish -> Fish -> Fish -> Fish -> Fish

data Torus a = Torus (Vector (Vector a)) Int Int
  deriving (Eq,Functor)

torusWH :: Torus a -> (Int,Int)
torusWH (Torus rows _ _) = (V.length (rows ! 0), V.length rows)

readTorus :: Int -> Int -> Torus a -> a
readTorus i j t@(Torus rows di dj) = (rows ! j') ! i' where
  i' = (i + di) `mod` w
  j' = (j + dj) `mod` h
  (w,h) = torusWH t

rotateTorus :: Torus a -> Int -> Int -> Torus a
rotateTorus (Torus rows di dj) dx dy = Torus rows (di + dx) (dj + dy)

duplicateTorus :: Torus a -> Torus (Torus a)
duplicateTorus t = genTorus (torusWH t) (rotateTorus t)

genTorus :: (Int,Int) -> (Int -> Int -> a) -> Torus a
genTorus (w,h) g = Torus (V.generate h (\j -> V.generate w (\i -> g i j))) 0 0

instance Show a => Show (Torus a) where
  show = pprint

instance Show Fish where
  show c = [f2c c]


-- parser

getData = do
  ls <- fmap lines (readFile "input")
  let rows = V.fromList (map (V.fromList . parseLine) ls)
  return (Torus rows 0 0)

parseLine :: String -> [Fish]
parseLine = map c2fish

c2fish '.' = O
c2fish '>' = E
c2fish 'v' = V

-- pretty printer

pprint :: Show a => Torus a -> String
pprint (Torus rows _ _) = (unlines . map (\row -> concatMap show (V.toList row)) . V.toList) rows

f2c O = '.'
f2c E = '>'
f2c V = 'v'

-- example

ex :: Torus Fish
ex = Torus (V.fromList
  [V.fromList [V,O,O,O]
  ,V.fromList [O,O,O,O]
  ,V.fromList [E,E,E,O]
  ,V.fromList [O,O,V,O]]) 0 0




-- Apply rules to torus of fish

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

search :: Int -> Torus Fish -> Int
search n t =
  let t' = step cuke t in
  if t==t'
    then n+1
    else search (n+1) t'

main :: IO ()
main = print =<< pure . search 0 =<< getData
