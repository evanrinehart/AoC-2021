module Main where

import Prelude hiding (join, insert)

import Data.List
import Data.Char
import Data.Ord
import Data.Maybe
import Data.Map (Map)
import Control.Monad hiding (join)
import qualified Data.Map.Strict as M


data Code = Code !Char !Char deriving (Eq,Ord,Show)
type Rules = M.Map Code Char
type ATable = M.Map Code Int
type PTable = M.Map Char Int

letters = "HCSNPKOBFV"

astart :: ATable
astart = M.fromList [(Code a b, 0) | a <- letters, b <- letters]

pstart :: PTable
pstart = M.fromList (zip letters (repeat 0))

countChain :: String -> ATable -> ATable
countChain (c1:c2:xs) tab = countChain (c2:xs) $! M.adjust (+1) (Code c1 c2) tab
countChain _ tab = tab

main = do
  (initial, tabs, rules) <- getData
  let (_,final) = iterate (crunch rules) tabs !! 40
  mapM_ print (M.toList final)

crunch :: Rules -> (ATable,PTable) -> (ATable,PTable)
crunch rules (atab,ptab) = (foldl' (flip ($)) atab aupdates, ptab') where
  aupdates :: [ATable -> ATable]
  aupdates = M.foldMapWithKey g atab
  pupdates :: [PTable -> PTable]
  pupdates = M.foldMapWithKey h atab
  ptab' = foldl' (flip ($)) ptab pupdates
  g code@(Code a b) 0 = []
  g code@(Code a b) n =
    case M.lookup code rules of
      Just x -> [M.adjust (subtract n) code
                ,M.adjust (+n) (Code a x)
                ,M.adjust (+n) (Code x b)]
      Nothing -> error "wrong, all codes have rules (1)"
  h code 0 = []
  h code n = case M.lookup code rules of
      Just x -> [M.adjust (+n) x]
      Nothing -> error "wrong, all codes have rules (2)"

loadRules :: [((Char,Char),Char)] -> Rules
loadRules = M.fromList . map (\((a,b),c) -> (Code a b, c))

loadPop :: String -> PTable
loadPop str = foldl' (\tab c -> M.adjust (+1) c tab) pstart str


getData :: IO (String,(ATable,PTable),Rules)
getData = do
  ls <- fmap lines (readFile "input")
  let [[template],rs] = split "" ls
  let rules = map ((\[[c1,c2,' ','-'],[' ',c3]] -> (Code c1 c2,c3)) . split '>') rs
  return (template,(countChain template astart, loadPop template), M.fromList rules)

literalInsert rules (c1:c2:[]) = case lookup [c1,c2] rules of
  Just d  -> [c1,d,c2]
  Nothing -> error "didn't think this happened"
literalInsert rules (c1:c2:xs) = case lookup [c1,c2] rules of
  Just d -> c1:d:literalInsert rules (c2:xs)
  Nothing -> error "didn't think this happened"
literalInsert rules [] = []
literalInsert rules [x] = [x]


split :: Eq a => a -> [a] -> [[a]]
split s = go where
  go xs = case break (==s) xs of
    (chunk,[])   -> [chunk]
    (chunk,more) -> chunk : go (dropOnly s more)
  dropOnly s y@(x:xs) = if x == s then xs else y

