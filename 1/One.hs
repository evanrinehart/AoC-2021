module One where

crunch =
  length .
  filter id .
  map (uncurry (<)) .
  (zip <*> tail) .
  map (read :: String -> Int) .
  take 2000 .
  lines

main =
  print =<<
  pure . crunch =<<
  readFile "input"
