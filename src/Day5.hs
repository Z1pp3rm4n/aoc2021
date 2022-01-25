module Day5 where

import Tools
import Text.Megaparsec.Char (char, string)

main = do 
  vents <- getParsedLines parseLine "05"
  print (length . intersections. filter isStraight $ vents)
  print (length . intersections $ vents)

intersections :: [Line] -> [Point]
intersections = repeatElems . concatMap points

points :: Line -> [Point]
points (Line (Point x1 y1) (Point x2 y2)) = 
  zipWith Point [x1,x1+stepX .. x2] [y1,y1+stepY .. y2]
  where 
    stepX = sign (x2 - x1)
    stepY = sign (y2 - y1)

isStraight :: Line -> Bool
isStraight (Line (Point x1 y1) (Point x2 y2)) =
  x1 == x2 || y1 == y2

parsePoint :: Parser Point
parsePoint = do
  x <- integer
  char ','
  y <- integer
  return $ Point x y

parseLine :: Parser Line
parseLine = do
  p1 <- parsePoint
  string " -> "
  p2 <- parsePoint
  return $ Line p1 p2

sign :: Int -> Int
sign x 
  | x > 0 = 1
  | x == 0 = 0
  | otherwise = -1  