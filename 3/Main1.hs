module Main1 where

import Data.List   (transpose)
import Text.Printf (printf)

import Common (fromBinary, zeroCount)

crunch1 :: Char -> Char -> String -> Char
crunch1 zbit obit col
  | zeroCount col < 500 = zbit
  | otherwise           = obit

main = do
  columns <- fmap (transpose . lines) (readFile "input")

  let gamma   = (fromBinary . map (crunch1 '1' '0')) columns
  let epsilon = (fromBinary . map (crunch1 '0' '1')) columns

  printf "power = %d\n" (gamma * epsilon)
