module Main where

import Data.List
import Data.Char
import Data.Maybe
import Data.Map (Map)
import Control.Monad
import qualified Data.Map as M
import qualified Data.Set as S

type Paper = S.Set (Int,Int)
data FoldAlong = X Int | Y Int deriving Show

parseFold str =
  let [l,r] = split '=' str in
  case last l of
    'x' -> X (read r)
    'y' -> Y (read r)
    _   -> error ("bad fold: " ++ str)

getData :: IO (Paper, [FoldAlong])
getData = do
  ls <- fmap lines (readFile "input")
  let [ls1, ls2] = split "" ls
  let dots = map (\[a,b] -> (read a,read b)) . map (split ',') $ ls1
  let folds = map parseFold ls2
  return (S.fromList dots,folds)

foldLeft :: Int -> Paper -> Paper
foldLeft x p = l `S.union` mirrorR where
  mirrorR = S.map mirror $ S.filter (\(a,b) -> a > x) p
  l = S.filter (\(a,b) -> a < x) p
  mirror (a,b) = (a - 2*(a-x), b)

foldUp :: Int -> Paper -> Paper
foldUp y p = t `S.union` mirrorB where
  mirrorB = S.map mirror $ S.filter (\(a,b) -> b > y) p
  t = S.filter (\(a,b) -> b < y) p
  mirror (a,b) = (a, b - 2*(b-y))

go [] p = p
go (X x : folds) p = go folds (foldLeft x p)
go (Y y : folds) p = go folds (foldUp y p)

main = do
  (dots,fld:flds) <- getData
  let dots' = go (fld:flds) dots
  mapM_ putStrLn (render dots')

render :: Paper -> [String]
render p = map f [0..6] where
  f y = map g [0..40] where
    g x = if (x,y) `S.member` p then '#' else ' '



split :: Eq a => a -> [a] -> [[a]]
split s = go where
  go xs = case break (==s) xs of
    (chunk,[])   -> [chunk]
    (chunk,more) -> chunk : go (dropOnly s more)
  dropOnly s y@(x:xs) = if x == s then xs else y

