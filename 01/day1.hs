#!/usr/bin/env stack
-- stack --resolver lts-9.14 script

sumMatchingOffset :: Int -> [Int] -> Int
sumMatchingOffset k xs =
  foldr (\x acc -> if fst x == snd x then acc + fst x else acc) 0 xs'
  where n = length xs
        xs' = zip xs (take n . drop k . cycle $ xs)

toDigits :: String -> [Int]
toDigits = map (read . (:""))

main :: IO ()
main = do
  digitsStr <- getLine
  let digits = toDigits digitsStr
  putStrLn . show $ sumMatchingOffset 1 digits
  putStrLn . show $ sumMatchingOffset (length digits `div` 2) digits
