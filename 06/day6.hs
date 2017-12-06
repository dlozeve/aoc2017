#!/usr/bin/env stack
-- stack --resolver lts-9.14 script

import Data.List

distribute :: Int -> Int -> [Int]
distribute _ 0 = []
distribute n len =
  map (+1) (take k baselist) ++ (drop k baselist)
  where baselist = replicate len (n `div` len)
        k = n `mod` len

redistribute :: [Int] -> [Int]
redistribute xs =
  zipWith (+) d xs'
  where n = maximum xs
        (a,_:b) = span (< n) xs
        xs' = a ++ (0:b)
        d = let l = distribute n (length xs)
                k = length xs - length a - 1
            in (drop k l) ++ (take k l)

findCycle :: [Int] -> (Int, Int)
findCycle xs =
  aux 0 [] xs
  where aux k acc xs =
          case elemIndex xs' acc of
            Nothing -> aux (k+1) (xs':acc) xs'
            Just i -> (k+1, i + 1)
          where xs' = redistribute xs

main :: IO ()
main = do
  xs <- map read . words <$> getLine
  putStrLn . show $ findCycle xs
