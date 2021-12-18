module Main where

import Prelude hiding (join, insert)

import Data.List
import Data.Char
import Data.Ord
import Data.Maybe
import Data.Map (Map)
import Control.Monad hiding (join)
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import qualified Data.Set as S
import Data.Set (Set)

data Snail = Reg Int | Pair Snail Snail deriving Eq

data Exploding =
  ExplodingLeft Int Snail |
  ExplodingRight Int Snail | 
  ExplodingBoth Int Int Snail |
  Exploded Snail |
  NotExploding Snail
    deriving Show

fromExploding x = case x of
  ExplodingLeft n s -> s
  ExplodingRight n s -> s
  ExplodingBoth m n s -> s
  Exploded s -> s
  NotExploding s -> s

-- parsing
parse str = fst (parseSnail str)

parseSnail :: String -> (Snail, String)
parseSnail str@(c:cs) = if isDigit c
  then let (juice,box) = span isDigit str in (Reg (read juice), box)
  else parsePair str

parsePair :: String -> (Snail, String)
parsePair ('[':input) =
  let (smoke, rest1) = parseSnail input
      rest2 = takeA ',' rest1
      (fire, rest3) = parseSnail rest2
      rest4 = takeA ']' rest3
  in (Pair smoke fire, rest4)

takeA c (d:ds) = if c==d then ds else error "takeA failed"

split :: Eq a => a -> [a] -> [[a]]
split s = go where
  go xs = case break (==s) xs of
    (chunk,[])   -> [chunk]
    (chunk,more) -> chunk : go (dropOnly s more)
  dropOnly s y@(x:xs) = if x == s then xs else y

pp (Reg n) = show n
pp (Pair a b) = "[" ++ pp a ++ "," ++ pp b ++ "]"

instance Show Snail where show = pp








-- crunch

explode :: Int -> Snail -> Exploding
explode 4 (Pair (Reg m) (Reg n)) = ExplodingBoth m n (Reg 0)
explode i (Reg m) = NotExploding (Reg m)
explode i (Pair a b) = case explode (i+1) a of
  ExplodingLeft n a'   -> ExplodingLeft n (Pair a' b)
  ExplodingRight n a'  -> Exploded (Pair a' (addLeft b n))
  ExplodingBoth m n a' -> ExplodingLeft m (Pair a' (addLeft b n))
  Exploded a'          -> Exploded (Pair a' b)
  NotExploding _ -> case explode (i+1) b of
    ExplodingLeft n b' -> Exploded (Pair (addRight a n) b')
    ExplodingRight n b' -> ExplodingRight n (Pair a b')
    ExplodingBoth m n b' -> ExplodingRight n (Pair (addRight a m) b')
    Exploded b' -> Exploded (Pair a b')
    NotExploding _ -> NotExploding (Pair a b)

addLeft (Reg n) m = Reg (n + m)
addLeft (Pair a b) m = Pair (addLeft a m) b
addRight (Reg n) m = Reg (n + m)
addRight (Pair a b) m = Pair a (addRight b m)

ssplit (Pair a b) =
  let (a',r1) = ssplit a in
  let (b',r2) = ssplit b in
  if r1
    then (Pair a' b, True)
    else if r2
      then (Pair a b', True)
      else (Pair a b, False)
ssplit (Reg n)
  | n < 10 = (Reg n, False)
  | n >= 10  = (Pair (Reg (n `div` 2)) (Reg (sop2 n)), True)

sop2 n | even n = n `div` 2
       | odd n  = n `div` 2 + 1

reduce :: Snail -> Snail
reduce x = case explode 0 x of
  NotExploding _ -> case ssplit x of
    (x', True) -> reduce x'
    (_, False) -> x
  other -> reduce (fromExploding other)

add :: Snail -> Snail -> Snail
add a b = reduce (Pair a b)

mag :: Snail -> Int
mag (Reg n) = n
mag (Pair a b) = 3 * mag a + 2 * mag b





-- IO

getData :: IO [Snail]
getData = do
  ls <- fmap lines (readFile "input")
  return (map (fst . parseSnail) ls)

main = do
  snails <- getData
  let pairs = [(x,y) | x <- snails, y <- snails, x /= y]
  let finals = map (uncurry add) pairs
  let mags = map mag finals
  let ans = maximum mags
  print ans
  --mapM_ print mags

testAdd str1 str2 = add (parse str1) (parse str2)

