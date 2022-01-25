module Day2 where

import Tools
import Text.Megaparsec.Char (string, hspace)
import Text.Megaparsec (choice)
import Data.Foldable (foldl')

type Command = (Direction,Int)
data Direction = Forward | Down | Up

main = do
  commands <- getParsedLines parseCommand "02"
  print (mult  (foldl' follow (0,0) commands))
  print (mult2  (foldl' follow2 (0,0,0) commands))


follow :: (Int, Int) -> Command -> (Int,Int)
follow (pos,depth) command = case command of
  (Forward,x) -> (pos + x,depth)
  (Down,x)    -> (pos,depth + x)
  (Up,x)      -> (pos,depth - x)

follow2 :: (Int, Int, Int) -> Command -> (Int ,Int ,Int)
follow2 (pos,depth,aim) command = case command of
  (Forward,x) -> (pos + x,depth + aim*x,aim)
  (Down,x)    -> (pos,depth,aim + x)
  (Up,x)      -> (pos,depth,aim - x)

parseCommand :: Parser Command
parseCommand = do
  dir <- parseDirection
  hspace
  steps <- integer
  return (dir,steps)

parseDirection :: Parser Direction
parseDirection = choice
  [Forward <$ string "forward"
  , Down <$ string "down"
  , Up <$ string "up"]

mult (x,y) = x * y
mult2 (x,y,z) = x * y