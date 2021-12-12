{-# LANGUAGE TupleSections #-}

module Day11 where

import Prelude hiding ((!!))
import GHC.OldList ((\\))
import Tools
import Data.Foldable (foldl')
import Data.Map.Strict (Map, (!), adjust)
import qualified Data.Map.Strict as Map
import Text.Megaparsec (sepBy, many)
import Text.Megaparsec.Char (eol)
import Data.Char (intToDigit)
import Data.Set (Set, insert, size)

main = do
  octopi <- getParsedInput parser "11"
  print (p1 octopi)
  print (p2 octopi)

p1,p2 :: Octopi -> Int
p1 oct = snd . simulate 100 $ (oct,0)

p2 oct = earliestSynchronized (oct,0) 0 

earliestSynchronized (oct,fl) day = 
  if newFl - fl == oSize^2 then day + 1
  else earliestSynchronized (newOct,newFl) (day+1)
  where 
    (newOct,newFl) = step (oct,fl)
    allOct = oSize ^ 2


type Octopi = Map (Int,Int) Int

prettyPrint :: Octopi -> IO ()
prettyPrint oct = putStrLn $ concatMap showLine range
  where
    showLine y = '\n' : map (\x -> intToDigit (oct ! (x,y))) range

parser :: Parser Octopi
parser = do
  arr2 <- many digit `sepBy` eol
  return $ createOctopi arr2

createOctopi :: [[Int]] -> Octopi
createOctopi arr2 = mx
  where
    mx = foldl' (\map pos -> Map.insert pos (arr2 !!! pos) map) mempty poses

oSize = 10

range = [0..oSize - 1]
poses = [(x,y) | x <- range, y <- range]

inBounds :: (Int,Int) -> Bool
inBounds (x,y) = 0 <= x && x < oSize && 0 <= y && y < oSize

neighbors :: (Int,Int) -> [(Int,Int)]
neighbors (x,y) = filter inBounds (region \\ [(x,y)])
  where
    region = [(x_,y_) | x_ <- [x-1..x+1], y_ <- [y-1..y+1]]

simulate :: Int -> (Octopi,Int) -> (Octopi,Int)
simulate days = fpow days step

step :: (Octopi,Int) -> (Octopi,Int)
step (oct,fl) = (reseted, fl + size flashed)
  where 
    reseted = foldl' (\l r -> Map.insert r 0 l) newOct flashed
    (newOct, flashed) = foldl' inc (oct,mempty) poses
    

inc :: (Octopi, Set (Int,Int)) -> (Int,Int) -> (Octopi, Set (Int,Int))
inc (mx, flashed) pos
  | newMx ! pos > 9 && pos `notElem` flashed = foldl' inc (newMx, insert pos flashed) (neighbors pos)
  | otherwise = (newMx, flashed)
  where
    newMx = adjust (+1) pos mx