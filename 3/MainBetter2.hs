module Main where

main = do
  words <- fmap lines (readFile "input")

  -- look at first columns to get initial partition
  let firstPart = part 0 words -- words is used up now

  -- drill the two parts starting at 2nd column
  let o2  = drill oxygenDecide 1 (oxygenDecide firstPart)
  let co2 = drill co2Decide    1 (co2Decide    firstPart)

  putStrLn ("life support " ++ show (o2 * co2))


-- partition a list depending on bit in ith place and remember counts
type Partition = (Int,[String],Int,[String])

part :: Int -> [String] -> Partition
part i ws = partWorker [] [] 0 0 i ws where
  partWorker zwords owords z o i [] = (z,zwords,o,owords)
  partWorker zwords owords z o i (w:ws) = case w !! i of
    '0' -> partWorker zwords (w:owords) (z+1) o i ws
    '1' -> partWorker (w:zwords) owords z (o+1) i ws

-- keep picking partition winners until 1 word left
drill :: (Partition -> [String]) -> Int -> [String] -> Int
drill decide i []  = error "no more things"
drill decide i [w] = fromBinary w
drill decide i ws  = drill decide (i+1) (decide (part i ws))

-- decide winner of a partition in one of two ways
oxygenDecide :: Partition -> [String]
oxygenDecide (z,zwords,o,owords) = case compare z o of
  LT -> owords
  EQ -> owords
  GT -> zwords

co2Decide :: Partition -> [String]
co2Decide (z,zwords,o,owords) = case compare z o of
  LT -> zwords
  EQ -> zwords
  GT -> owords

-- -- --
fromBinary :: String -> Int
fromBinary = go . reverse . dropWhile (=='0') where
  go [] = 0
  go ('1':[])   = 1
  go ('0':bits) = go bits * 2
  go ('1':bits) = go bits * 2 + 1
