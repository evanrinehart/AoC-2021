module Two where

add3 (x,y,z) = x + y + z

crunch1A =
  length .
  filter id .
  map (uncurry (<)) .
  (zip <*> tail)

crunch2 =
  map add3 .
  (\ns -> zip3 ns (tail ns) (tail (tail ns)))

crunch1B =
  map (read :: String -> Int) .
  take 2000 .
  lines

main = fmap (crunch1A . crunch2 . crunch1B) (readFile "input") >>= print


