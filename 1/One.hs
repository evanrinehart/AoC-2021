module One where

crunch =
  length .
  filter id .
  map (uncurry (<)) .
  (zip <*> tail) .
  map (read :: String -> Int) .
  take 2000 .
  lines

main = fmap crunch (readFile "input") >>= print
