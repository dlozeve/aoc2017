#!/usr/bin/env stack
-- stack --resolver lts-9.18 script

import Data.List
import Data.List.Split
import Data.Char
import Data.Bits
import Text.Printf

twist :: [Int] -> Int -> Int -> [Int]
twist xs len skip =
  end ++ start
  where (knot, keep) = splitAt len xs
        (start, end) = splitAt skip $ keep ++ reverse knot

twists :: [Int] -> [Int] -> Int -> [Int]
twists list [] k = list
twists list (x:xs) k = twists (twist list x k) xs (k+1)

hashRound :: Int -> [Int] -> [Int]
hashRound n lens = drop rot list ++ take rot list
  where list = foldl' (\acc (x,k) -> twist acc x k) [0..(n-1)] $ zip lens (cycle [0..(n-1)])
        rot = n - (sum (zipWith (+) [0..] lens) `rem` n)

sparseHash :: [Int] -> [Int]
sparseHash lengths =
  hashRound 256 . concat . replicate 64 $ lengths ++ [17, 31, 73, 47, 23]

denseHash :: [Int] -> [Int]
denseHash h = foldr xor 0 <$> chunksOf 16 h

grid :: String -> [[Int]]
grid input =
  map (denseHash . sparseHash . map ord . ((input++"-")++) . show) [0..127]

main :: IO ()
main = do
  let input = "stpzcrnm"
  let g = grid input
  mapM_ (putStrLn . concatMap (printf "%08b")) g
  print . length . filter (=='1') . concat $ map (concatMap (printf "%08b")) g
