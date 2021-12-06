module Main where

import System.Process
import System.IO
import Control.Monad

type Table = [(Int,Int)]

main = do
  ns <- fmap (map snd . load . map read . split ',') (readFile "input")
  print =<< outsource "./crunch" 80 ns
  print =<< outsource "./crunch" 256 ns

outsource :: FilePath -> Int -> [Int] -> IO Int
outsource cmd days ns = do

  let config = (proc cmd []){ std_out = CreatePipe, std_in = CreatePipe }
  (Just stdin, Just stdout, _, process) <- createProcess config

  hPutStrLn stdin (show days)
  forM_ ns $ \n -> do
    hPutStrLn stdin (show n)
  hClose stdin

  answer <- fmap read (hGetLine stdout)
  waitForProcess process
  return answer

load :: [Int] -> Table
load [] = blank
load (n:ns) = modAt n (\(foo,bar) -> (foo,bar+1)) (load ns)

blank   = [(0,0),(1,0),(2,0),(3,0),(4,0),(5,0),(6,0),(7,0),(8,0)]
example = [(0,0),(1,1),(2,1),(3,2),(4,1),(5,0),(6,0),(7,0),(8,0)]

modAt :: Int -> (a -> a) -> [a] -> [a]
modAt 0 f (x:xs) = f x : xs
modAt i f (x:xs) = x : modAt (i-1) f xs

split :: Eq a => a -> [a] -> [[a]]
split s = go where
  go xs = case break (==s) xs of
    (chunk,[])   -> [chunk]
    (chunk,more) -> chunk : go (dropOnly s more)
  dropOnly s y@(x:xs) = if x == s then xs else y

