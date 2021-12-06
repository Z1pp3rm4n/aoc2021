module Tools where

import Text.Megaparsec (Parsec, parse, errorBundlePretty)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void (Void)
import Data.List (group, sort)

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

integer :: Parser Int
integer = L.decimal

double :: Parser Double
double = L.float

-----------

repeatElems :: Ord a => [a] -> [a]
repeatElems = map head . filter ((>1) . length) . group .sort

countElem :: Eq a => a -> [a] -> Int
countElem x = length . filter (== x) 

fpow :: Int -> (a -> a) -> a -> a
fpow n f x = iterate f x !! n

data Point = Point Int Int deriving (Eq, Show)
data Line = Line Point Point deriving (Eq, Show)

instance Ord Point where
  compare (Point x1 y1) (Point x2 y2) =
    if (y1 == y2) then compare x1 x2
    else compare y1 y2