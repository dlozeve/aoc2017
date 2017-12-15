#!/usr/bin/env stack
-- stack --resolver lts-9.18 script

import Data.Bits
import Data.List

genA :: Integral a => a -> a
genA n = (16807 * n) `rem` 2147483647

genB :: Integral a => a -> a
genB n = (48271 * n) `rem` 2147483647

matchLowest16Bits :: (Bits a, Num a) => a -> a -> Bool
matchLowest16Bits n1 n2 =
  n1 .&. 0xFFFF == n2 .&. 0xFFFF

main :: IO ()
main = do
  let (inputA, inputB) = (783, 325) :: (Int, Int)
  let n = 40000000
  let seqA = take n $ iterate genA inputA
  let seqB = take n $ iterate genB inputB
  print . foldl' (flip ((+) . fromEnum)) 0 $ zipWith matchLowest16Bits seqA seqB
  let n' = 5000000
  let seqA' = take n' . filter (\k -> k `rem` 4 == 0)
              $ iterate genA inputA
  let seqB' = take n' . filter (\k -> k `rem` 8 == 0)
              $ iterate genB inputB
  print . foldl' (flip ((+) . fromEnum)) 0 $ zipWith matchLowest16Bits seqA' seqB'
