module Day10 where

import Tools
import Data.List (sort, foldl')

main = do
  lines <- getLines "10"
  print (p1 lines)
  print (p2 lines)

p1 = sum . map score1
p2 lines = sorted !! median
  where
    sorted = sort . filter (/= 0) . map score2 $ lines
    median = length sorted `div` 2

close :: Char -> Char
close c = case c of
  '(' -> ')'
  '[' -> ']'
  '{' -> '}'
  '<' -> '>'

points1 c = case c of
  ')' -> 3
  ']' -> 57
  '}' -> 1197
  '>' -> 25137

points2 c = case c of
  ')' -> 1
  ']' -> 2
  '}' -> 3
  '>' -> 4

isOpen c = c `elem` "([{<"

score1,score2 :: [Char] -> Int
score1 s = case parse s of
  Left c -> points1 c
  Right _ -> 0

score2 s = case parse s of
  Left c -> 0
  Right cs -> foldl' (\l r -> l*5 + points2 (close r)) 0 cs

parse :: [Char] -> Either Char [Char]
parse = f []
  where
    f stack [] = Right stack
    f [] (c:cs) = if isOpen c then f [c] cs else Left c
    f stack@(s:ss) (c:cs)
      | isOpen c = f (c:stack) cs
      | c == close s = f ss cs
      | otherwise = Left c