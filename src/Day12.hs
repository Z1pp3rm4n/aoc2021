module Day12 where

import Tools
import Data.Map.Internal (Map, alter, (!))
import Data.List.Split (splitOn)
import Data.Char (isLower, isUpper)
import Data.Foldable (foldl')

type Labyrinth = Map Node [Node]
type Node = String

main = do
  lab <- parse <$> getInput "12"
  print (p1 lab)
  print (p2 lab)

p1 :: Labyrinth -> Int
p1 lab = length (paths lab notVisited ["start"])
  where
    notVisited path n = (isBig n) || (n `notElem` path)

p2 lab = length (paths lab visitTwice ["start"])
  where
    visitTwice path "end" = True
    visitTwice path "start" = False
    visitTwice path n
      | isBig n = True
      | otherwise = n `notElem` path || not (hasRepeat path)

    hasRepeat = not . null . repeatElems . filter isSmall

paths :: Labyrinth -> ([Node] -> Node -> Bool) -> [Node] -> [[Node]]
paths lab f path
  | lastNode == "end" = [path]
  | otherwise = concatMap (\n -> paths lab f (n:path)) newNodes
  where
    newNodes = filter (f path) (lab ! lastNode)
    lastNode = head path

isBig = all isUpper

isSmall :: Node -> Bool
isSmall = all isLower

parse :: String -> Labyrinth
parse s = foldl' parseLine mempty (lines s)

parseLine :: Labyrinth -> String -> Labyrinth
parseLine lab line = (addN n1 n2) . (addN n2 n1) $ lab
  where
    (n1:n2:_) = splitOn "-" line


addN :: Node -> Node -> Labyrinth  -> Labyrinth
addN n1 n2 = alter f n1
  where
    f Nothing = Just [n2]
    f (Just ns) = Just (n2:ns)