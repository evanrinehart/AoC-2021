{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
module Main where

import Data.Maybe
import Data.List
import Data.Foldable
import Data.String
import Data.Set as S (Set,toList,fromList,delete)

main = do
  d <- getData
  print $ sum $ map (\(ss,my) -> solveLine ss my) d
  return ()


solveLine pats mystery =
  let tab = solve pats start in
  grok $ map (val . flip decode tab) mystery



grok [a,b,c,d] = a*1000 + b*100 + c*10 + d

getData = do
  let g [l,r] = (split ' ' l, split ' ' r)
  fmap (map g . map (map trim) . map (split '|') . lines) (readFile "input")

trim :: String -> String
trim xs = dropWhile isSpace (reverse (dropWhile isSpace (reverse xs)))

split :: Eq a => a -> [a] -> [[a]]
split s = go where
  go xs = case break (==s) xs of
    (chunk,[])   -> [chunk]
    (chunk,more) -> chunk : go (dropOnly s more)
  dropOnly s y@(x:xs) = if x == s then xs else y

isSpace = (==' ')

part1OfLine :: String -> Int
part1OfLine raw = 
  let [_, r] = split '|' raw in
  let r' = trim r in
  let sizes = map length (split ' ' r') in
  let wanted = map isWanted sizes in
  length (filter id wanted)


isWanted 2 = True
isWanted 3 = True
isWanted 7 = True
isWanted 4 = True
isWanted _ = False









-- table for each char, what it could possible map to
type Table = [(Char,[Char])]
start = map (\c -> (c,anyLetter)) anyLetter
anyLetter = "abcdefg"

mapTable :: (Char -> [Char] -> [Char]) -> Table -> Table
mapTable f tab = map (\(c,ss) -> (c, f c ss)) tab

like1 c = c `elem` "cf"
like4 c = c `elem` "bcdf"
like7 c = c `elem` "acf"



-- when these two appear, then 1 (cf) shows
-- so x->c y->f OR x->f y->c  ?
learn1 :: Char -> Char -> Table -> Table
learn1 c1 c2 tab = mapTable f tab where
  f c xs | c==c1 || c==c2 = filter like1 xs
         | otherwise      = filter (not . like1) xs

learn4 :: Char -> Char -> Char -> Char -> Table -> Table
learn4 c1 c2 c3 c4 tab = mapTable f tab where
  f c xs | c `elem` [c1,c2,c3,c4] = filter like4 xs
         | otherwise              = filter (not . like4) xs

learn7 :: Char -> Char -> Char -> Table -> Table
learn7 c1 c2 c3 tab = mapTable f tab where
  f c xs | c `elem` [c1,c2,c3] = filter like7 xs
         | otherwise           = filter (not . like7) xs

-- mark c1 maps to c2 (cross pattern)
definitely :: Char -> Char -> Table -> Table
definitely c1 c2 tab = mapTable f tab where
  f c xs | c == c2   = filter (==c1) xs
         | otherwise = filter (/=c1) xs

isSolved :: Table -> Bool
isSolved tab = all singular (map snd tab)

singular :: [a] -> Bool
singular []  = False
singular [x] = True
singular _   = False

applyTable :: Table -> Char -> [Char]
applyTable tab c = case lookup c tab of
  Nothing -> error "bad table"
  Just xs -> xs

reverseTable :: Table -> Table
reverseTable tab = map f "abcdefg" where
  f c = (c, catMaybes (map (g c) "abcdefg"))
  g c d = case c `elem` applyTable tab d of
    False -> Nothing
    True  -> Just d

possibleMappingsTo :: Char -> String -> Table -> [Char]
possibleMappingsTo c str tab =
  applyTable tab c `intersect` str

-- if a 5ino (which must contain g) has g in one possible place
-- then you know what maps to g, else you know nothing new
--   (if nothing could map to g, 
learn5g :: String -> Table -> Table
learn5g five tab = case possibleMappingsTo 'g' five tab of
  [c] -> definitely c 'g' tab
  _   -> tab

learn5d :: String -> Table -> Table
learn5d five tab = case possibleMappingsTo 'd' five tab of
  [c] -> definitely c 'd' tab
  _   -> tab

learn5a :: String -> Table -> Table
learn5a five tab = case possibleMappingsTo 'a' five tab of
  [c] -> definitely c 'a' tab
  _   -> tab

learn6a :: String -> Table -> Table
learn6a six tab = case possibleMappingsTo 'a' six tab of
  [c] -> definitely c 'a' tab
  _   -> tab

learn6b :: String -> Table -> Table
learn6b six tab = case possibleMappingsTo 'b' six tab of
  [c] -> definitely c 'b' tab
  _   -> tab

learn6f :: String -> Table -> Table
learn6f six tab = case possibleMappingsTo 'f' six tab of
  [c] -> definitely c 'f' tab
  _   -> tab

learn6g :: String -> Table -> Table
learn6g six tab = case possibleMappingsTo 'g' six tab of
  [c] -> definitely c 'g' tab
  _   -> tab


learn5e :: String -> Table -> Table
learn5e five tab = case possibleMappingsTo 'e' five tab of
  [c] -> definitely c 'e' tab



-- for any word, if there is only 1 place c could appear in the mapping
-- then that is where it appears and 1 mapping is definitely known

rule1 :: Char -> String -> Table -> Table
rule1 c str rtab = case possibleMappingsTo c str rtab of
  [d] -> definitely d c rtab
  _   -> rtab


think c ss rtab = foldl (flip ($)) rtab (map (rule1 c) ss)
thonk ss rtab = foldl (flip ($)) rtab (map (\d -> think d ss) "abcdefg")

tka = rule1 'a'
tkb = rule1 'b'
tkc = rule1 'c'
tkd = rule1 'd'
tke = rule1 'e'
tkf = rule1 'f'
tkg = rule1 'g'



go :: [String] -> Table -> Table
go [] tab = tab
go (s:ss) tab = case length s of
  2 -> let [c1,c2] = s in go ss (learn1 c1 c2 tab)
  3 -> let [c1,c2,c3] = s in go ss (learn7 c1 c2 c3 tab)
  4 -> let [c1,c2,c3,c4] = s in go ss (learn4 c1 c2 c3 c4 tab)
  5 -> go ss tab
  6 -> go ss tab
  7 -> go ss tab

go2 :: [String] -> Table -> Table
go2 [] rtab = rtab
go2 (s:ss) rtab = case length s of
  2 -> go2 ss rtab
  3 -> go2 ss rtab
  4 -> go2 ss rtab
  5 -> go2 ss (tkg s (tkd s (tka s rtab)))
  6 -> go2 ss (tka s (tkg s (tkb s (tkf s rtab))))
  7 -> go2 ss rtab


solve ss = reverseTable . go2 ss . reverseTable . go ss

decode p tab = map f p where
  f c = case lookup c tab of
    Just [d] -> d
    _ -> error "bad table"
   
val :: String -> Int
val p = case sort p of
  "abcefg"  -> 0
  "cf"      -> 1
  "acdeg"   -> 2
  "acdfg"   -> 3
  "bcdf"    -> 4
  "abdfg"   -> 5
  "abdefg"  -> 6
  "acf"     -> 7
  "abcdefg" -> 8
  "abcdfg"  -> 9

