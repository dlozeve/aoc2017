#!/usr/bin/env stack
-- stack --resolver lts-9.14 script

range :: (Ord a, Num a) => [a] -> a
range l = maximum l - minimum l

divisors :: (Integral a) => [a] -> a
divisors [] = 0
divisors (x:xs) =
  case filter (\y -> (x `mod` y == 0) || (y `mod` x == 0)) xs of
    [] -> divisors xs
    y:_ -> if y > x then y `div` x
           else x `div` y

main :: IO ()
main = do
  contents <- getContents
  let numbers = map (map read . words) $ lines contents :: [[Int]]
  putStrLn . show $ sum $ map range numbers
  putStrLn . show $ sum $ map divisors numbers
