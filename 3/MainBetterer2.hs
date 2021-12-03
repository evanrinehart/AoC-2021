module Main where

import Data.IntSet as IntSet

data Block = Block Int IntSet -- keybase numset
  deriving Show

type Partition = (Block,Block)

main = do
  everything <- fmap (IntSet.fromList . fmap fromBinary . lines) (readFile "input")

  let block1 = Block 0 everything
  let part1 = fission (2 ^ 11) block1

  let o2  = drill oxygenDecide 1 (oxygenDecide part1)
  let co2 = drill co2Decide    1 (co2Decide    part1)

  putStrLn ("life support " ++ show (o2 * co2))

-- split a block
fission :: Int -> Block -> Partition
fission bit (Block base set) =
  let k = base + bit in
  let (zs,found,os) = IntSet.splitMember k set in
  if found
    then (Block base zs, Block k (IntSet.insert k os))
    else (Block base zs, Block k os)

-- iteratively fission a block into two, at bit i, decide winner
drill :: (Partition -> Block) -> Int -> Block -> Int
drill decide i stuff =
  let lessStuff@(Block _ set) = decide (fission (2 ^ (11 - i)) stuff) in
  case minView set of
    Nothing -> error "no more numbers"
    Just (n, ns) -> case minView ns of
      Nothing -> n
      Just _  -> drill decide (i + 1) lessStuff

-- decide winner of a partition in one of two ways
oxygenDecide :: Partition -> Block
oxygenDecide (bz@(Block _ zs), bo@(Block _ os)) = case compare (IntSet.size zs) (IntSet.size os) of
  LT -> bo
  EQ -> bo
  GT -> bz

co2Decide :: Partition -> Block
co2Decide (bz@(Block _ zs), bo@(Block _ os)) = case compare (IntSet.size zs) (IntSet.size os) of
  LT -> bz
  EQ -> bz
  GT -> bo

-- -- --
fromBinary :: String -> Int
fromBinary = go . reverse . dropWhile (=='0') where
  go [] = 0
  go ('1':[])   = 1
  go ('0':bits) = go bits * 2
  go ('1':bits) = go bits * 2 + 1
