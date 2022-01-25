module Day6 where

import Tools
import Text.Megaparsec (sepBy)
import Text.Megaparsec.Char (char)
import Data.List (foldl', group, sort)
import Data.Map.Strict (Map, fromList)

main = do
  fishes <- getParsedInput parseFishes "06"
  print (total. simulate 80 $ fishes)
  print (total. simulate 256 $ fishes)

type Fishes = [(Integer,Integer)]

total :: Fishes -> Integer
total = sum . map snd


parseFishes :: Parser Fishes
parseFishes = do
  nums <- integer `sepBy` char ','
  return . map toInteger2 . groupByCount $ nums

groupByCount :: Ord a => [a] -> [(a, Int)]
groupByCount = map (\l -> (head l,length l)) . group . sort

toInteger2 (a,b) = (toInteger a, toInteger b)

simulate days = fpow days step

step :: Fishes -> Fishes
step fs = map (\day -> (day, findNewValue day fs)) days
  where
    days = [0..8]

findNewValue :: Integer -> Fishes -> Integer
findNewValue day fs = case day of 
  6 -> getCount 0 fs + getCount 7 fs
  8 -> getCount 0 fs
  _ -> getCount (day + 1) fs
  
  

getCount :: Integer -> Fishes -> Integer
getCount day fs = case lookup day fs of
  Just cnt -> cnt
  _ -> 0    
    