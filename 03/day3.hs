#!/usr/bin/env stack
-- stack --resolver lts-9.14 script

import qualified Data.Map.Strict as Map
import Data.Maybe

right, up, left, down :: (Int,Int) -> (Int,Int)
right (a,b) = (a+1,b  )
up    (a,b) = (a  ,b+1)
left  (a,b) = (a-1,b  )
down  (a,b) = (a  ,b-1)

spiral :: [(Int, Int)]
spiral = scanl (flip ($)) (0,0) directions
  where directions = concat $
          zipWith replicate (concat (map (replicate 2) [1..])) $
          cycle [right, up, left, down]

shortestPathLength :: Int -> Int
shortestPathLength n =
  let (x,y) = spiral !! (n-1) in
    (abs x) + (abs y)

neighbours :: (Int,Int) -> [(Int,Int)]
neighbours (a,b) =
  [(a+k,b+l) | k <- [-1..1], l <- [-1..1]]


insertCell :: (Int,Int) -> Map.Map (Int,Int) Int -> Map.Map (Int,Int) Int
insertCell (a,b) m =
  Map.insert (a,b) (fromJust sumNeighbours) m
  where sumNeighbours = fmap sum $ sequence . (filter isJust) $ fmap (\k -> Map.lookup k m) (neighbours (a,b))

spiralSum :: Int -> Map.Map (Int,Int) Int
spiralSum n = foldl (flip insertCell) (Map.singleton (0,0) 1) (take n (tail spiral))

spiralSumValues :: Int -> [Int]
spiralSumValues n = fromJust . sequence $ fmap (\x -> Map.lookup x (spiralSum n)) (take n spiral)

firstValueLarger :: Int -> Int
firstValueLarger n = head $ filter (> n) (spiralSumValues (n `div` 1000))

main :: IO ()
main = do
  input <- getLine
  let n = read input :: Int
  putStrLn . show $ shortestPathLength n
  putStrLn . show $ firstValueLarger n

