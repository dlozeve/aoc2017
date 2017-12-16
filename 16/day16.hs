#!/usr/bin/env stack
-- stack --resolver lts-9.18 script

{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import qualified Data.Text as T
import Data.Attoparsec.Text
import Control.Applicative
import Data.List
import Data.Maybe
import Data.Monoid

data Move = Spin Int | Exchange Int Int | Partner Char Char
  deriving (Eq, Show)

isPartner :: Move -> Bool
isPartner (Partner _ _) = True
isPartner _ = False

parseSpin :: Parser Move
parseSpin = string "s" *> decimal >>= return . Spin

parseExchange :: Parser Move
parseExchange = do
  string "x"
  a <- decimal
  string "/"
  b <- decimal
  return $ Exchange a b

parsePartner :: Parser Move
parsePartner = do
  string "p"
  a <- letter
  string "/"
  b <- letter
  return $ Partner a b

parseMove :: Parser Move
parseMove = parseSpin <|> parseExchange <|> parsePartner

parseMoves :: Parser [Move]
parseMoves = parseMove `sepBy` (skipSpace *> string "," *> skipSpace)

spin :: Int -> [a] -> [a]
spin n xs = end ++ start
  where (start, end) = splitAt (length xs - n) xs

exchange :: Int -> Int -> [a] -> [a]
exchange m n xs
  | m == n = xs
  | m > n = exchange n m xs
  | otherwise = start ++ b:as ++ a:bs
  where (start, end) = splitAt m xs
        (a:as, b:bs) = splitAt (n-m) end

partner :: Eq t => t -> t -> [t] -> [t]
partner a b xs = exchange m n xs
  where Just m = elemIndex a xs
        Just n = elemIndex b xs

move :: [Char] -> Move -> [Char]
move xs (Spin n) = spin n xs
move xs (Exchange m n) = exchange m n xs
move xs (Partner a b) = partner a b xs

findRepetition :: Eq a => [a] -> Maybe Int
findRepetition [] = Nothing
findRepetition (x:xs) = case elemIndices x xs of
  [] -> Nothing
  inds -> Just $ last inds

moveMany 0 _ xs _ = xs
moveMany n seen xs (m:ms) =
  if xs `elem` seen
  then concat $ intersperse "\n" seen-- !! (length seen - n `rem` length seen)
  else let xs' = move xs m in
         moveMany (n-1) (xs:seen) xs' ms

newtype Perm16 = Perm16 [Int]
  deriving (Show, Eq)

toPermutation :: Move -> Perm16
toPermutation (Spin n) = Perm16 $ spin n [0..15]
toPermutation (Exchange m n) = Perm16 $ exchange m n [0..15]
toPermutation (Partner _ _) = Perm16 [0..15]

instance Monoid Perm16 where
  mempty = Perm16 [0..15]
  mappend (Perm16 p1) (Perm16 p2) = Perm16 $ map (p1 !!) p2

applyPerm :: [b] -> Perm16 -> [b]
applyPerm xs (Perm16 p) = map (xs !!) p

main :: IO ()
main = do
  contents <- T.pack <$> getContents
  let Right moves = parseOnly parseMoves contents
  let start = ['a'..'p']
  let partnerMoves = filter isPartner moves
  -- print (length moves)
  let positions = scanl' move start . concat $ replicate 1 moves
  putStrLn $ last positions
  let dance = mconcat . map toPermutation $  moves
  putStrLn $ foldl' move (applyPerm start dance) partnerMoves
  let thousanddance = iterate (<> dance) dance !! 1000
  let milliondance = iterate (<> thousanddance) thousanddance !! 1000
  let billiondance = iterate (<> milliondance) milliondance !! 1000
  print billiondance
  let billionPartnerMoves = concat $ replicate 1000000000 partnerMoves
  putStrLn $ foldl' move (applyPerm start billiondance) billionPartnerMoves
