module Common where

fromBinary :: String -> Int
fromBinary = go . reverse . dropWhile (=='0') where
  go [] = 0
  go ('1':[])   = 1
  go ('0':bits) = go bits * 2
  go ('1':bits) = go bits * 2 + 1

zeroCount  = length . filter (=='0')

halfLength :: [a] -> Int
halfLength = (`div` 2) . length 
