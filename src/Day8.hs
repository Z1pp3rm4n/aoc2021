module Day8 where

import Data.Set (Set, size, fromList, isSubsetOf)
import Tools
import Data.Functor.Classes (liftEq)
import Data.Map (Map, (!), toList, insert, (!?))
import qualified Data.Map as Map (fromList,size,empty)
import Text.Megaparsec (sepBy,many, some)
import Text.Megaparsec.Char (digitChar, space1, string, letterChar, space, char)
import Data.Foldable (foldl')
import Data.List.Split (splitOn)

type Pattern = Set Char

main = do
  lines <- map parseLine <$> getLines "08"
  print $ p1 lines
  print $ p2 lines

p1,p2 :: [([Pattern], [Pattern])] -> Int
p1 lines = sum . map (`countElem` decoded) $ [1,4,7,8]
  where decoded = concatMap decode lines

p2 = sum . map (digitsToInt . decode)

toSet :: [Char] -> Pattern
toSet = fromList

digitsToInt :: [Int] -> Int
digitsToInt = foldl' (\acc d -> acc * 10 + d) 0

parsePattern :: Parser Pattern
parsePattern = fromList <$> some letterChar

parseLine :: String -> ([Pattern],[Pattern])
parseLine = parse . splitOn " | "
  where parse (patterns:outputs:_) = (map fromList . words $ patterns, map fromList . words $ outputs)

decode :: ([Pattern], [Pattern]) -> [Int]
decode (patterns,output) = map (patternToInt !) output
  where patternToInt = process patterns

process :: [Pattern] -> Map Pattern Int
process = processIter Map.empty

processIter :: Map Int Pattern -> [Pattern] -> Map Pattern Int
processIter iToP ps = if Map.size newItoP == 10
  then reverseMap newItoP
  else processIter newItoP ps
  where
    newItoP = foldl' findValue iToP ps

findValue :: Map Int Pattern -> Pattern -> Map Int Pattern
findValue iToP p = case size p of
  2 -> put 1
  4 -> put 4
  3 -> put 7
  7 -> put 8
  5
    | false (contains 1) && false (containedIn 9) -> put 2
    | true (contains 1) && true (containedIn 9) -> put 3
    | false (contains 1) && true (containedIn 9) -> put 5
  6
    | true (contains 1) && false (contains 4) -> put 0
    | true (contains 1) && true (contains 4) -> put 9
    | false (contains 1)                      -> put 6
  _ -> iToP
  where
    put x = insert x p iToP

    contains i = (`isSubsetOf` p) <$> (iToP !? i)
    containedIn i = (p `isSubsetOf`) <$> (iToP !? i)

    true (Just True) = True
    true _ = False
    false (Just False) = True
    false _ = False

reverseMap :: (Ord a, Ord b) => Map a b -> Map b a
reverseMap =  Map.fromList . map (\(k,v) -> (v,k)). toList
