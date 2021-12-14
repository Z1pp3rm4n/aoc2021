module Day13 where

import Tools
import Data.Set.Internal (Set, fromList, findMax)
import qualified Data.Set.Internal as Set
import Data.List.Split (splitOn)
import Data.List (nub, foldl')

type Paper = Set Point
data Fold = Side Int | Up Int

main = do
  input <- parse <$> getInput "13"
  print (p1 input)
  p2 input

p1 :: (Paper,[Fold]) -> Int
p1 (paper,folds) = length (foldPaper paper first)
  where
    first = head folds

p2 :: (Paper,[Fold]) -> IO()
p2 (paper,folds) = prettyPrint $ foldl' foldPaper paper folds   

prettyPrint :: Paper -> IO()
prettyPrint paper = putStrLn $ concatMap showLine [0..yMax]
  where
    showLine yLine = [showPoint (Point x_ yLine) | x_ <- [0 .. xMax]] ++ "\n"
    showPoint = \p -> if p `elem` paper then '#' else '.'
    xMax = findMax (Set.map x paper)
    yMax = findMax (Set.map y paper)


foldPaper :: Paper -> Fold -> Paper
foldPaper paper f = Set.map fold paper
  where
    fold old@(Point x y) = case f of
      Side x_ -> if x > x_ then Point (2*x_ - x) y else old
      Up y_   -> if y > y_ then Point x (2*y_ - y) else old

parse :: String -> (Paper,[Fold])
parse = p . splitOn "\n\n"
  where
    p (paper:folds:_) = (fromList . map parsePoint . lines $ paper, map parseFold . lines $folds)

parsePoint :: String -> Point
parsePoint s = Point (read x) (read y)
  where
    (x:y:_) = splitOn "," s

parseFold :: String -> Fold
parseFold s =
  if head info == 'x' then Side num
  else Up num
  where
    info = drop 11 s
    num = read (drop 2 info)