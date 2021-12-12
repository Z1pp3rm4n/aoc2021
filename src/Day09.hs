module Day09 where

import Tools
import Text.Megaparsec.Char (digitChar, newline)
import Text.Megaparsec (some, sepBy)
import Data.Maybe(catMaybes, mapMaybe, fromJust)
import Data.Char (digitToInt)
import Data.List (nub, sort)
import Data.Set (fromList, Set, toList, foldl', union)
import qualified Data.Set as Set

main = do
  lava <- getParsedInput parserLava "09"
  print (p1 lava)
  print (map (findBasin lava) . lowestPoints $ lava)
  print (p2 lava)

p1,p2 :: [[Int]] -> Int
p1 mx = sum . map ((+1) . (mx !-!)) . lowestPoints $ mx

p2 mx = product $ map (sorted !!) [0..2]
  where
    sorted = reverse (sort basinSizes)
    basinSizes = map (length . findBasin mx) . lowestPoints $ mx

parserLava :: Parser [[Int]]
parserLava = some (digitToInt <$> digitChar ) `sepBy` newline

findBasin :: [[Int]] -> (Int,Int) -> [(Int,Int)]
findBasin mx low = toList . bfs . fromList $ [low]
  where
    bfs :: Set (Int,Int) -> Set (Int,Int)
    bfs set = if set == newSet
      then set
      else bfs newSet
      where
        newSet = foldl' (\s p -> s `union` nSet p)  set set
        nSet = Set.filter ((/= 9) . (mx !-!)) . fromList . neighbors mx 

lowestPoints :: [[Int]] -> [(Int,Int)]
lowestPoints mx = filter (isLowest mx) points
  where
    points = [(x,y) | x <- [0..width mx - 1], y <- [0..height mx - 1]]

isLowest :: [[Int]] -> (Int,Int) -> Bool
isLowest mx pos = all (> val) ns
  where
    ns = map (mx !-!)  (neighbors mx pos)
    val = mx !-! pos

neighbors :: [[Int]] -> (Int,Int) -> [(Int,Int)]
neighbors mx (x,y) = filter (inBounds mx) poses
  where poses = [(x-1,y), (x+1,y), (x,y-1),(x,y+1)]

inBounds :: [[Int]] -> (Int,Int) -> Bool
inBounds mx (x,y) = 0 <= x && x < w && 0 <= y && y < h
  where
      w = width mx
      h = height mx

(!-!) :: [[Int]] -> (Int,Int) ->  Int
(!-!) matrix (x,y) = (matrix !! y) !! x

width,height :: [[Int]] -> Int
width = length . head
height = length