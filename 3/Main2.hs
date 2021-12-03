module Main where

import Text.Printf (printf)

import Common (fromBinary, zeroCount, halfLength)

main = do
  words <- fmap lines (readFile "input")
  let o2  = crunch2 '1' 0 words
  let co2 = crunch2 '0' 0 words
  printf "life support = %d\n" (o2 * co2)

crunch2 :: Char -> Int -> [String] -> Int
crunch2 zbit i []    = error "no more things"
crunch2 zbit i [w]   = fromBinary w
crunch2 zbit i stuff = crunch2 zbit (i+1) lessStuff where
  lessStuff = filter (bitCrit . (!! i)) stuff
  col_i     = map (!! i) stuff
  bitCrit   = if zeroCount col_i <= halfLength stuff
                then (==zbit)
                else (/=zbit)
