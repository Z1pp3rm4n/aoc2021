module Day14 where

import Tools
import Data.Map.Internal (Map, (!), insertWith, foldlWithKey, empty, toList)
import qualified Data.Map.Internal as Map
import Text.Megaparsec (count, many, sepBy)
import Text.Megaparsec.Char (letterChar, string, eol)
import Data.List (group, sort)
import Data.Foldable (foldl')
import Control.Arrow (second)

type Rules = Map Pair Char
type Template = Map Pair Integer
type Pair = (Char,Char)

main = do 
  input <- getParsedInput parser "14"
  print (p1 input)
  print (p2 input)
  

p1 :: (Rules, String) -> Integer
p1 = maxMinDiff 10

p2 = maxMinDiff 40

maxMinDiff :: Int -> (Rules, String) -> Integer
maxMinDiff times (rules,str) = maximum elemCounts - minimum elemCounts
  where
    temp = toTemplate str
    inserted = fpow times (step rules) temp
    elemCounts = toCounts str inserted

step :: Rules -> Template -> Template
step rules = foldlWithKey (f_ rules) empty

f_ :: Rules -> Template -> Pair -> Integer -> Template
f_ rules temp pair cnt = inc (ch1,inf) cnt . inc (inf,ch2) cnt $ temp
  where
    (ch1,ch2) = pair
    inf = rules ! pair

inc :: Ord a => a -> Integer -> Map a Integer -> Map a Integer
inc = insertWith (+)

toTemplate :: String -> Template
toTemplate s = Map.fromList (countsInteger pairs)
  where
    pairs = zip s (tail s)

toCounts :: String -> Template -> Map Char Integer
toCounts s = half . incFirstAndLast . gather
  where
    gather = foldlWithKey (\table (c1,c2) cnt -> inc c1 cnt . inc c2 cnt $ table) mempty
    incFirstAndLast = inc (head s) 1 . inc (last s) 1
    half = fmap (`div` 2)

countsInteger :: Ord a => [a] -> [(a,Integer)]
countsInteger = map (second toInteger) . counts

parser :: Parser (Rules,String)
parser = do
  template <- many letterChar
  string "\n\n"
  rules <- Map.fromList <$> (parseRule `sepBy` eol)
  return (rules,template)

parseRule :: Parser (Pair,Char)
parseRule = do
  ch1 <- letterChar
  ch2 <- letterChar
  string " -> "
  inf   <- letterChar
  return ((ch1,ch2),inf)

