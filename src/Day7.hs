module Day7 where

import Tools
import Text.Megaparsec (sepBy, many)
import Text.Megaparsec.Char (char, digitChar)

main = do
  crabs <- getParsedInput parseCrabs "07"
  print (best id crabs)
  print (best (\n -> n*(n+1) `div` 2) crabs)


parseCrabs :: Parser [Int]
parseCrabs = integer `sepBy` char ','

range :: [Int] -> [Int]
range crabs = [0..maximum crabs]

cost :: [Int] -> (Int -> Int) -> Int -> Int
cost crabs f pos = sum . map (f . abs . subtract pos) $ crabs

best :: (Int -> Int) -> [Int] -> Int
best f crabs = minimum . map (cost crabs f ) $ range crabs
