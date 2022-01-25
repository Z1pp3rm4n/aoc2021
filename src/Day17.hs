module Day17 where

import Tools

-- assumptions : minY, maxY < 0

legalVelocities :: (Point,Point) -> [Point]
legalVelocities rect@(Point minX minY , Point maxX maxY) 
  = filter (isLegal rect) velocities
  where
    velocities = [Point x y | x <- [0..maxVx], y <- [minVy..maxVy]]
    minVy = minY
    maxVy = -minY - 1
    maxVx = maxX

inRect :: (Point,Point) -> Point -> Bool
inRect (Point minX minY , Point maxX maxY) (Point x y)
  = minX <= x && x <= maxX
  && minY <= y && y <= maxY

beyondRect :: (Point,Point) -> Point -> Bool
beyondRect (Point minX minY , Point maxX maxY) (Point x y) =
  x > maxX || y < minY

isLegal :: (Point,Point) -> Point -> Bool
isLegal rect vel = any (inRect rect) consideredPositions
  where
    consideredPositions = takeWhile (not . beyondRect rect) positions
    positions = map fst . iterate step $ (start,vel)
    start = Point 0 0


sim :: Int -> (Point, Point) -> (Point, Point)
sim t = fpow t step

step :: (Point,Point) -> (Point,Point)
step (Point sx sy, Point vx vy) = (newS,newV)
  where
    newS = Point (sx + vx) (sy + vy)
    newV = Point newVx (vy - 1)
    newVx = if vx == 0 then vx else vx - 1
