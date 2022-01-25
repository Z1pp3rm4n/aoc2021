module Day18 where

import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe, fromJust)
import Tools
import Text.Megaparsec.Char (char)
import Data.Foldable (maximumBy)

data Tree a = Leaf a | Fork (Tree a) (Tree a) deriving (Show,Eq)
data Context a = Top | L (Context a) (Tree a) | R (Context a) (Tree a) deriving Show
type Location a = (Tree a, Context a) 

main = do 
  trees <- getParsedLines pTree "18"
  print (p1 trees)
  print (p2 trees)

p1,p2 :: [Tree Int] -> Int
p1 = magnitude . foldl1 snailAdd
p2 trees = maximum . map (magnitude . uncurry snailAdd) $ pairs
  where 
    pairs = [(t1,t2) | t1 <- trees, t2 <- trees, t1 /= t2 ]

pTree :: Parser (Tree Int)
pTree = parseLeaf <|> parseFork
  where
    parseLeaf = Leaf <$> integer
    parseFork = do
      char '['
      l <- pTree
      char ','
      r <- pTree
      char ']'
      return $ Fork l r

snailAdd :: Tree Int -> Tree Int -> Tree Int
snailAdd t1 t2 = reduce (Fork t1 t2)

magnitude :: Tree Int -> Int
magnitude (Fork l r) = 3 * magnitude l + 2 * magnitude r
magnitude (Leaf x) = x

toTree :: Location a -> Tree a
toTree = fst . most up

reduce :: Tree Int -> Tree Int
reduce tree = maybe tree reduce (explode tree <|> split tree)

split :: Tree Int -> Maybe (Tree Int)
split tree = do
  toSplit <- leftMostSatisfy splitable loc
  return $ toTree . modify splitLeaf $ toSplit --
  where
    loc = top tree
    splitLeaf (Leaf x) = Fork (Leaf (x `div` 2)) (Leaf (x - x `div` 2))

explode :: Tree Int -> Maybe (Tree Int)
explode tree = do
  loc <- leftMostSatisfy explodable (top tree)
  let
    (Fork (Leaf l) (Leaf r), _) = loc
    center = snd loc
    loc0 = deleteNode loc
    loc1 = try (addNearestLeft l) loc0
    loc2 = (fromJust . goTo center) . (most up) $ loc1
    loc3 = try (addNearestRight r) loc2
  return $ toTree loc3

deleteNode = modify (\ _ -> Leaf 0)
addNearestLeft n node = modify (\(Leaf x) -> Leaf (x+n)) <$> nearestLeft node
addNearestRight n node = modify (\(Leaf x) -> Leaf (x+n)) <$> nearestRight node
   

goTo :: Context a -> Location a -> Maybe (Location a)
goTo Top loc = Just loc
goTo (L c _) loc = (goTo c loc) >>= left
goTo (R c _) loc = (goTo c loc) >>= right


nearestLeft :: Location Int -> Maybe (Location Int)
nearestLeft loc = (most right) <$> firstLeftAunt loc

nearestRight loc = (most left) <$> firstRightAunt loc

firstLeftAunt :: Location a -> Maybe (Location a)
firstLeftAunt (_,Top) = Nothing
firstLeftAunt loc@(_, L _ _) = up loc >>= firstLeftAunt
firstLeftAunt loc@(_, R _ _) = up loc >>= left

firstRightAunt :: Location a -> Maybe (Location a)
firstRightAunt (_,Top) = Nothing
firstRightAunt loc@(_, R _ _) = up loc >>= firstRightAunt
firstRightAunt loc@(_, L _ _) = up loc >>= right


toMaybe :: (a -> Bool) -> a -> Maybe a
toMaybe f a = if f a then Just a else Nothing


leftMostSatisfy :: (Location a -> Bool) -> Location a -> Maybe (Location a)
leftMostSatisfy f loc@(Leaf _, _) = toMaybe f loc
leftMostSatisfy f loc@(Fork _ _, _) =
  (left loc >>= leftMostSatisfy f)
  <|> toMaybe f loc
  <|> (right loc >>= leftMostSatisfy f)

splitable :: Location Int -> Bool
splitable (Leaf x,_) = x >= 10
splitable _ = False

explodable :: Location Int -> Bool
explodable (t, c) = case t of
  Fork (Leaf _) (Leaf _) -> depth c == 4
  _ -> False

depth :: Context a -> Int
depth Top = 0
depth (L c _) = 1 + depth c
depth (R c _) = 1 + depth c

left :: Location a -> Maybe (Location a)
left (Fork l r, c) = Just (l, L c r)
left _ = Nothing

right :: Location a -> Maybe (Location a)
right (Fork l r, c) = Just (r, R c l)
right _ = Nothing

top :: Tree a -> Location a
top t = (t, Top)

up :: Location a -> Maybe (Location a)
up (t, L c r) = Just (Fork t r, c)
up (t, R c l) = Just (Fork l t, c)
up _ = Nothing

modify :: (Tree a -> Tree a) -> Location a -> Location a
modify f (t,c) = (f t, c)

most :: (a -> Maybe a) -> a ->  a
most f x = case f x of
  Just x' -> most f x'
  Nothing -> x

try :: (a -> Maybe a) -> a -> a
try f x = fromMaybe x (f x)

firstUp :: (Location a -> Maybe (Location a)) -> Location a -> Maybe (Location a)
firstUp _ (_,Top) = Nothing
firstUp dirF loc = (up loc >>= dirF) <|> (up loc >>= firstUp dirF)

