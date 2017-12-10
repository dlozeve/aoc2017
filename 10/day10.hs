#!/usr/bin/env stack
-- stack --resolver lts-9.17 script

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
hashRound n lens = (drop rot list) ++ (take rot list)
  where list = foldl' (\acc (x,k) -> twist acc x k) [0..(n-1)] $ zip lens (cycle [0..(n-1)])
        rot = n - (sum (zipWith (+) [0..] lens) `rem` n)

sparseHash :: [Int] -> [Int]
sparseHash lengths =
  hashRound 256 . concat . replicate 64 $ lengths ++ [17, 31, 73, 47, 23]

denseHash :: [Int] -> [Int]
denseHash h = foldr xor 0 <$> chunksOf 16 h

main :: IO ()
main = do
  line <- getLine
  let lengths = map read . splitOn "," $ line
  -- let a:b:_ = hashRound 5 [3,4,1,5]
  -- putStrLn . show $ a*b
  let a:b:_ = hashRound 256 lengths
  putStrLn . show $ a*b
  let h = denseHash . sparseHash . map ord $ line
  putStrLn . concat . map (printf "%02x") $ h
