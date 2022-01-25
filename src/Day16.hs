module Day16 where

import Tools
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec hiding (getInput)
import Text.Megaparsec.Char
import Data.Foldable (foldl')
import Data.Char (digitToInt)
import Numeric (readHex)
import Text.Printf (printf)
import Foreign.Marshal (fromBool)

data Packet = Operator Int Int [Packet] -- version -- typeid -- subpackets
  | Literal Int Int -- version - value
  deriving Show

main = do
  input <- getInput "16"
  let 
    binary = concatMap hexToBin input
    packet = myParse pPacket binary
  print (p1 packet)
  print (p2 packet)

p1 :: Packet -> Int
p1 = versionSum

p2 :: Packet -> Int
p2 = eval

versionSum :: Packet -> Int
versionSum (Operator ver _ subPackets) = ver + (sum . map versionSum $ subPackets)
versionSum (Literal ver _) = ver

eval :: Packet -> Int
eval (Literal _ val) = val
eval (Operator _ typeId subPackets) = 
  case typeId of
    0 -> sum subVals
    1 -> product subVals
    2 -> minimum subVals
    3 -> maximum subVals
    5 -> fromBool (subVals !! 0 > subVals !! 1)
    6 -> fromBool (subVals !! 0 < subVals !! 1)
    7 -> fromBool (subVals !! 0 == subVals !! 1)
  where subVals = map eval subPackets  
    

hexToBin :: Char -> String
hexToBin c = case readHex [c] of
      (x,_):_ -> printf "%04b" (x::Int)

fromBin :: String -> Int
fromBin = foldl' (\l r -> l*2 + r) 0 . map digitToInt

pBinary :: Int -> Parser Int
pBinary binLength = fromBin <$> count binLength binDigitChar

pPacket :: Parser Packet
pPacket = try pLiteral <|> pOperator

pOperator :: Parser Packet
pOperator = do
  version <- pBinary 3
  typeId  <- pBinary 3
  lengthTypeId <- binDigitChar
  if lengthTypeId == '0' then do
    subPacketsLength <- pBinary 15
    subPacketsString <- count subPacketsLength binDigitChar
    let
      subPackets = myParse (some pPacket) subPacketsString
    return $ Operator version typeId subPackets
  else do
    subPacketsCount <- pBinary 11
    subPackets <- count subPacketsCount pPacket
    return $ Operator version typeId subPackets


pLiteral :: Parser Packet
pLiteral = do
  version <- fromBin <$> (count 3 binDigitChar)
  string "100"
  value <- pValue
  return $ Literal version value

pValue :: Parser Int
pValue = do
  groups <- many parseGroup
  lastGroup <- parseLast
  let
    fullLit = concat (groups ++ [lastGroup])
  return $ fromBin fullLit
  where
    parseGroup = char '1' >> count 4 binDigitChar
    parseLast = char '0' >> count 4 binDigitChar