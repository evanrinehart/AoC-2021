{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Monad.State


data Player = Player
  { plSpace :: Space
  , plScore :: Int } deriving Show

data Game = Game
  { gameCounter :: Int
  , gamePlayer1 :: Player
  , gamePlayer2 :: Player
  , gameDice :: Dice } deriving Show

newtype Dice = Dice { getDice :: Int } deriving Show
newtype Space = Space { getSpace :: Int } deriving Show

onDice f game = game{ gameDice = f (gameDice game) }
onCounter f game = game{ gameCounter = f (gameCounter game) }
onP1 f game = game{ gamePlayer1 = f (gamePlayer1 game) }
onP2 f game = game{ gamePlayer2 = f (gamePlayer2 game) }
onPlayerSpace f pl = pl{ plSpace = f (plSpace pl) }
onPlayerScore f pl = pl{ plScore = f (plScore pl) }

initialState :: Space -> Space -> Game
initialState start1 start2 =
  Game {
    gameCounter = 0,
    gameDice = Dice 1,
    gamePlayer1 = Player { plSpace = start1, plScore = 0 },
    gamePlayer2 = Player { plSpace = start2, plScore = 0 }
  }

rollDice :: State Game Int
rollDice = do
  Dice n <- gets gameDice
  modify $ onDice (\(Dice n) -> if n == 100 then Dice 1 else Dice (n + 1))
  modify $ onCounter (+1)
  return n

boardGameMovement :: Int -> Space -> Space
boardGameMovement n (Space x) = Space $ foldl (\y () -> if y == 10 then 1 else y + 1) x (replicate n ())

moveNP1 :: Int -> State Game ()
moveNP1 n = do
  modify $ onP1 (onPlayerSpace (boardGameMovement n))
  Space x <- gets (plSpace . gamePlayer1)
  modify $ onP1 (onPlayerScore (+x))

moveNP2 :: Int -> State Game ()
moveNP2 n = do
  modify $ onP2 (onPlayerSpace (boardGameMovement n))
  Space x <- gets (plSpace . gamePlayer2)
  modify $ onP2 (onPlayerScore (+x))

isGameOver :: State Game Bool
isGameOver = do
  score1 <- gets (plScore . gamePlayer1)
  score2 <- gets (plScore . gamePlayer2)
  return (score1 >= 1000 || score2 >= 1000)

player1Turn :: State Game Int
player1Turn = do
  n <- fmap sum $ replicateM 3 rollDice
  moveNP1 n
  isGameOver >>= \case
    True  -> getAnswer
    False -> player2Turn

player2Turn :: State Game Int
player2Turn = do
  n <- fmap sum $ replicateM 3 rollDice
  moveNP2 n
  isGameOver >>= \case
    True  -> getAnswer
    False -> player1Turn

getAnswer :: State Game Int
getAnswer = do
  c <- gets gameCounter
  score1 <- gets (plScore . gamePlayer1)
  score2 <- gets (plScore . gamePlayer2)
  if score1 >= 1000 && not (score2 >= 1000)
    then return (c * score2)
    else if score2 >= 1000 && not (score1 >= 1000)
      then return (c * score1)
      else error "getAnswer failed"

main = do
  (start1, start2) <- getData
  let answer = runState player1Turn (initialState start1 start2)
  print answer

getData = do
  [l1,l2] <- fmap lines $ readFile "input"
  let f str = let [_,(_:more)] = split ':' str in read more
  let x1 = f l1
  let x2 = f l2
  return (Space x1, Space x2)

split :: Eq a => a -> [a] -> [[a]]
split s = go where
  go xs = case break (==s) xs of
    (chunk,[])   -> [chunk]
    (chunk,more) -> chunk : go (dropOnly s more)
  dropOnly s y@(x:xs) = if x == s then xs else y


