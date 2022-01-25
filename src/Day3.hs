module Day3 where

import Data.Foldable (foldl')
import Data.List (transpose)
import Tools
import Data.Char (digitToInt)

main = do
  report <- map (map digitToInt) <$> getLines "03"
  let g = fromBin . gamma $ report
      e = fromBin . epsilon $ report
  print (g*e)
  let o = fromBin . oxygen $ report
      c = fromBin . co2 $ report
  print (o*c)    

oxygen report = loop report 0 mostCommonBit
co2 report = loop report 0 leastCommonBit

loop :: [[Int]] -> Int -> ([[Int]] -> Int -> Int) -> [Int]
loop [x] _ _ = x
loop report pos f = loop filtered (pos + 1) f
  where
    filtered = filter ((== bit) . (!! pos)) report
    bit = f report pos

gamma :: [[Int]] -> [Int]
gamma report = map (mostCommonBit report) [0..width-1]
  where width = length (head report)

epsilon = invert . gamma

mostCommonBit :: [[Int]] -> Int -> Int
mostCommonBit report pos =
  if count1 >= count0 then 1 else 0
  where
    column = transpose report !! pos
    count1 = countElem 1 column
    count0 = countElem 0 column

leastCommonBit report pos = 1 - mostCommonBit report pos

invert = map (1 -)

fromBin :: [Int] -> Int
fromBin = foldl' (\acc x -> acc*2 + x) 0