{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Monad.State.Strict
import Data.Either
import qualified Data.Map as M
import Data.Map.Strict (Map)
import Debug.Trace

data Game = Game
  { gP1x :: !Int
  , gP2x :: !Int
  , gP1s :: !Int
  , gP2s :: !Int
  , gWho :: !Who } deriving (Eq,Ord,Show,Read)

data Who = P1 | P2 deriving (Eq,Ord,Show,Read,Enum)

type Populations = Map Game Int

toggle P1 = P2
toggle P2 = P1

initialState :: Int -> Int -> Game
initialState start1 start2 =
  Game { gP1x = start1, gP2x = start2, gP1s = 0, gP2s = 0, gWho = P1 }

boardGameMovement :: Int -> Int -> Int
boardGameMovement n x = foldl (\y () -> if y == 10 then 1 else y + 1) x (replicate n ())

moveNP1 :: Int -> State Game ()
moveNP1 n = do
  x <- gets (boardGameMovement n . gP1x)
  s <- gets gP1s
  modify $ \g -> g{ gP1x = x, gP1s = s + x }

moveNP2 :: Int -> State Game ()
moveNP2 n = do
  x <- gets (boardGameMovement n . gP2x)
  s <- gets gP2s
  modify $ \g -> g{ gP2x = x, gP2s = s + x }

isGameOver :: State Game Bool
isGameOver = do
  score1 <- gets gP1s
  score2 <- gets gP2s
  return (score1 >= 21 || score2 >= 21)

doTurn :: Int -> Int -> Int -> State Game (Maybe Who)
doTurn a b c = do
  let n = a + b + c
  who <- gets gWho
  case who of P1 -> moveNP1 n; P2 -> moveNP2 n
  isGameOver >>= \case
    True  -> fmap Just getWinner
    False -> do
      modify $ \g -> g { gWho = toggle (gWho g)}
      return Nothing

getWinner :: State Game Who
getWinner = do
  score1 <- gets gP1s
  score2 <- gets gP2s
  if score1 >= 21 && not (score2 >= 21)
    then return P1
    else if score2 >= 21 && not (score1 >= 21)
      then return P2
      else do
        game <- get
        error ("???" ++ show game)

runTurn :: Int -> Int -> Int -> Game -> Either Game Who
runTurn a b c game = case runState (doTurn a b c) game of
  (Nothing, next)  -> Left next
  (Just winner, _) -> Right winner

diracs = [(a,b,c) | a <- [1..3], b <- [1..3], c <- [1..3]]

waysP1WinsNow :: Game -> Int
waysP1WinsNow game = sum (map f diracs) where
  f (a,b,c) = case runTurn a b c game of Right P1 -> 1; _ -> 0

waysP2WinsNow :: Game -> Int
waysP2WinsNow game = sum (map f diracs) where
  f (a,b,c) = case runTurn a b c game of Right P2 -> 1; _ -> 0

gameContinues :: Int -> Game -> Populations -> Populations
gameContinues n game base = foldl f base diracs where
  f tab (a,b,c) = case runTurn a b c game of Left next -> M.insertWith (+) next n tab; _ -> tab

play :: Int -> Int -> Populations -> (Int,Int)
play w1 w2 pops = case M.null pops of
  True -> (w1,w2)
  False ->
    let more1s = M.foldlWithKey' (\acc game count -> acc + count * waysP1WinsNow game) 0 pops
        more2s = M.foldlWithKey' (\acc game count -> acc + count * waysP2WinsNow game) 0 pops
        pops'  = M.foldlWithKey' (\tab game count -> gameContinues count game tab) M.empty pops
    in play (w1+more1s) (w2+more2s) pops'

main = do
  (start1, start2) <- getData
  let game = initialState start1 start2
  let pops = M.fromList [(game,1)]
  let (c1,c2) = (play 0 0 pops)
  print (c1, c2, max c1 c2)

getData = do
  [l1,l2] <- fmap lines $ readFile "input"
  let f str = let [_,(_:more)] = split ':' str in read more
  return (f l1, f l2)

split :: Eq a => a -> [a] -> [[a]]
split s = go where
  go xs = case break (==s) xs of
    (chunk,[])   -> [chunk]
    (chunk,more) -> chunk : go (dropOnly s more)
  dropOnly s y@(x:xs) = if x == s then xs else y


