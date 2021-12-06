{-# LANGUAGE TupleSections #-}

module Day4 where

import Tools
import Text.Megaparsec (sepBy, sepBy1)
import Text.Megaparsec.Char (char, string, hspace)
import Data.List (transpose, (\\))
import Data.List.Split (splitOn)

type Board = [[Entry]]
type Entry = (Int,Bool)
type Draw = [Int]

main = do
  (draw,boards) <- parse <$> getInput "04"
  print (head (playBingo draw boards))
  print (last (playBingo draw boards))

parse :: String -> (Draw,[Board])
parse s = (parseDraw draw, map parseBoard boards)
  where
    (draw:boards) = splitOn "\n\n" s
    parseDraw = map read . splitOn ","
    parseBoard = map (map ((,False) . read) . words) . lines

playBingo :: Draw -> [Board] -> [Int]
playBingo [] _ = []
playBingo (x:xs) boards = scores ++ playBingo xs remaining
  where
    remaining = (marked \\ won)
    won = filter win marked
    scores = map (score . (,x)) won
    marked = map (mark x) boards

mark :: Int -> Board -> Board
mark num = map2 (\old@(x,_) ->
  if x == num then (x, True)
  else old)

win :: Board -> Bool
win board = anyRow board || anyCol board
  where
    anyCol = anyRow . transpose
    anyRow = any (all snd)

score :: (Board,Int) -> Int
score (board,x) = sum unmarked * x
  where
    unmarked = map fst . filter (not . snd) . concat $ board

map2 :: (a -> b) -> [[a]] -> [[b]]
map2 f = map (map f)