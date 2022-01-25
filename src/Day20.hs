module Day20 where

import Tools
import Data.Map.Internal (Map, findWithDefault, keysSet, fromSet, fromList)
import qualified Data.Map.Internal as M
import Data.Foldable (foldl')

type Image = Map Point Char

main = do 
  input@(algo,img) <- parse <$> getInput "20"
  putStrLn (show (getEnhancePixel '.' algo img (Point 0 0)))
  print (p1 input)
  print (p2 input)

parse :: String -> ([Char],Image)
parse = getInfo . lines
  where
    getInfo (algo:_:img) = (algo, to2DMap img)
         
countLit = M.size . M.filter (== '#') 

p1 :: ([Char],Image) -> Int
p1 = countLit . uncurry (enhance 2)

p2 = countLit . uncurry (enhance 50)

enhance :: Int -> [Char] -> Image -> Image
enhance n algo img = enhanceWithDef '.' xMin xMax yMin yMax n algo img
  where
    ((xMin,xMax),(yMin,yMax)) = getRange2D (M.keysSet img)

enhanceWithDef :: Char -> Int -> Int -> Int -> Int -> Int -> [Char] -> Image -> Image
enhanceWithDef _ _ _ _ _ 0 _ img = img
enhanceWithDef def xMin xMax yMin yMax n algo img =
  enhanceWithDef newDef (xMin-1) (xMax+1) (yMin-1) (yMax+1) (n-1) algo newImg
  where
    newImg = M.fromList (map (\p -> (p, getEnhancePixel def algo img p)) newPoints)
    newDef = algo !! fromBinary (replicate 9 def)
    newPoints = [Point x y | x <- [xMin-1..xMax+1], y <- [yMin-1..yMax+1]]

getEnhancePixel :: Char -> [Char] -> Image -> Point -> Char
getEnhancePixel def algo img point = algo !! index
  where
    index = fromBinary (extractPortion def img point)

fromBinary :: [Char] -> Int
fromBinary = foldl' (\acc ch -> acc * 2 + if ch == '.' then 0 else 1) 0

extractPortion :: Char -> Image -> Point -> [Char]
extractPortion def img (Point x y) = map (\p -> findWithDefault def p img) points
  where
    points = [Point x_ y_ | y_ <- [y-1..y+1], x_ <- [x-1..x+1]]