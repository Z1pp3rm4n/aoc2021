module Tools where

import Text.Megaparsec (Parsec, parse, errorBundlePretty)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void (Void)
import Data.List (group, sort)
import Data.Char (digitToInt)
import Text.Megaparsec.Char (digitChar)
import Control.Arrow ((&&&))
import qualified Data.Map.Internal as Map
import qualified Data.Set as Set
import Data.Map.Internal (Map)
import Data.Foldable (foldl')
import Data.Map.Internal ((!))
import Data.Set (Set)

getInput :: String -> IO String
getInput fname = readFile ("input/" ++ fname)

getLines :: String -> IO [String]
getLines fname = lines <$> getInput fname

getParsedInput :: Parser a -> String -> IO a
getParsedInput p fname = myParse p <$> getInput fname

getParsedLines :: Parser a -> String -> IO [a]
getParsedLines p fname = map (myParse p) <$> getLines fname

myParse :: Parser a -> String -> a
myParse p s = case parse p "" s of
  Left e -> error (errorBundlePretty e)
  Right a -> a

type Parser = Parsec Void String

digit :: Parser Int
digit = digitToInt <$> digitChar

integer :: Parser Int
integer = L.decimal

double :: Parser Double
double = L.float


-----------

counts :: Ord a => [a] -> [(a,Int)]
counts = map (head &&& length) . group . sort

repeatElems :: Ord a => [a] -> [a]
repeatElems = map head . filter ((>1) . length) . group .sort

countElem :: Eq a => a -> [a] -> Int
countElem x = length . filter (== x)

fpow :: Int -> (a -> a) -> a -> a
fpow n f x = iterate f x !! n

data Point = Point {getX :: Int, getY :: Int} deriving (Eq, Show)
data Line = Line Point Point deriving (Eq, Show)

instance Ord Point where
  compare (Point x1 y1) (Point x2 y2) =
    if y1 == y2 then compare x1 x2
    else compare y1 y2

(!!!) :: [[a]] -> (Int,Int) -> a
mx !!! (x,y) = (mx !! y) !! x

to2DMap :: [[a]] -> Map Point a
to2DMap arr2 = Map.fromList pointsToElem
  where
    pointsToElem = map (\p@(Point x y) -> (p,arr2 !!! (x,y))) points
    width = length (head arr2)
    height = length arr2
    points = [Point x y | x <- [0..width-1], y <- [0..height-1]]

print2DMap :: Map Point Char -> IO ()
print2DMap pMap = putStrLn string
  where
    string = unlines (map showLine [yMin..yMax])
    showLine y = map (\x -> pMap ! Point x y) [xMin..xMax]
    points = Map.keysSet pMap
    ((xMin,xMax),(yMin,yMax)) = getRange2D points
    
getRange2D :: Set Point -> ((Int,Int),(Int,Int))
getRange2D points = ((xMin,xMax),(yMin,yMax))
  where 
    (xMin,xMax) = minAndMax (Set.map getX points)
    (yMin,yMax) = minAndMax (Set.map getY points)
    minAndMax = Set.findMin &&& Set.findMax