module Main where

import Data.List
import Data.Char
import Data.Maybe
import Data.Map (Map)
import Control.Monad
import qualified Data.Map as M
import qualified Data.Set as S


data Bigness = Big | Small deriving Show
type Cave = String
data Graph = G (Map Cave [Cave]) (Map Cave Bigness) deriving Show

exits :: Graph -> Cave -> [Cave]
exits (G exs big) c = case M.lookup c exs of
  Nothing -> error ("bad cave: " ++ show c)
  Just xs -> xs

isSmall :: Cave -> Graph -> Bool
isSmall c (G _ m) = case M.lookup c m of
  Nothing -> error ("(bigness) bad cave: " ++ show c)
  Just Small -> True
  Just Big -> False

bigness str =
  if all isLower str
    then Small
    else if all isUpper str
      then Big
      else error ("bad bigness: " ++ show str)

parseExits :: Cave -> [(Cave,Cave)] -> [Cave]
parseExits me dats = go dats where
  go [] = []
  go ((x,y):more) =
    if x==me
      then y : go more
      else if y==me
        then x : go more
          else go more

getData = do
  ls <- fmap lines (readFile "input")
  let lpairs = map (split '-') ls
  let pairs = map (\[x,y] -> (x,y)) lpairs
  let caves = nub (concat lpairs)
  let tab2 = M.fromList $ map (\c -> (c,bigness c)) caves
  let tab1 = M.fromList $ map (\c -> (c, parseExits c pairs)) caves
  let graph = G tab1 tab2
  return (graph, filter (\c -> isSmall c graph) caves)
  --return (paths "start" "end" graph)

paths :: Cave -> Cave -> Graph -> [[Cave]]
paths start end graph = go start [start] S.empty where
  go c acc doNotEnter = if c == end
    then return acc
    else do
      let dne' = (if isSmall c graph then S.insert c else id) doNotEnter
      d <- exits graph c
      if S.member d dne'
        then []
        else
          go d (d:acc) dne'

paths2 :: Cave -> Cave -> Cave -> Graph -> [[Cave]]
paths2 start end special graph = go start [start] S.empty False where
  go c acc doNotEnter flag = if c == end
    then return acc
    else do
      let flag' = flag || c==special
      let marking1 = c/=special && isSmall c graph
      let marking2 = c==special && flag
      let dne' = (if marking1 || marking2 then S.insert c else id) doNotEnter
      d <- exits graph c
      if S.member d dne'
        then []
        else
          go d (d:acc) dne' flag'

  
main = return ()



split :: Eq a => a -> [a] -> [[a]]
split s = go where
  go xs = case break (==s) xs of
    (chunk,[])   -> [chunk]
    (chunk,more) -> chunk : go (dropOnly s more)
  dropOnly s y@(x:xs) = if x == s then xs else y

