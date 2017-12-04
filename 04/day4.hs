#!/usr/bin/env stack
-- stack --resolver lts-9.14 script

import Data.List

testDuplicates :: [String] -> Bool
testDuplicates xs = length (nub xs) /= (length xs)

countLinesWithDuplicates :: [String] -> Int
countLinesWithDuplicates = sum . map (fromEnum . testDuplicates . words)

isAnagram :: String -> String -> Bool
isAnagram s1 s2 = sort s1 == sort s2

testAnagrams :: [String] -> Bool
testAnagrams [] = True
testAnagrams (x:xs) =
  if or (map (isAnagram x) xs)
  then False
  else testAnagrams xs

countValidLines :: [String] -> Int
countValidLines = sum . map (fromEnum . testAnagrams . words)

main :: IO ()
main = do
  contents <- getContents
  putStrLn . show . (\xs -> length xs - countLinesWithDuplicates xs) . lines $ contents
  putStrLn . show . countValidLines . lines $ contents
