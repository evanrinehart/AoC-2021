module Main where

getWord = takeWhile (/=' ')

getNum :: String -> Int
getNum = read . drop 1 . dropWhile (/=' ')

crunch1 (x,y) s =
  let w = getWord s in
  let n = getNum s in
  case w of
    "forward" -> (x+n, y)
    "down"    -> (x, y+n)
    "up"      -> (x, y-n)

crunch2 (x,y,a) s =
  let w = getWord s in
  let n = getNum s in
  case w of
    "forward" -> (x+n, y+(a*n), a)
    "down"    -> (x, y, a+n)
    "up"      -> (x, y, a-n)

main = do
  instructions <- lines <$> readFile "input"

  let (x,y) = foldl crunch1 (0,0) instructions
  print (x * y)

  let (x,y,a) = foldl crunch2 (0,0,0) instructions
  print (x * y)
