module Day1 where

import Tools
main = do
  depths <- getParsedLines integer "01"
  print (increases $ depths)
  print (increases . windows $ depths)

windows :: [Int] -> [Int]
windows (x:y:z:zs) = (x+y+z) : windows (y:z:zs)
windows _ = []

increases :: [Int] -> Int
increases = countElem GT . changes

changes :: Ord a => [a] -> [Ordering]
changes (x:y:ys) = compare y x : changes (y:ys)
changes _ = []
