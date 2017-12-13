#!/usr/bin/env stack
-- stack --resolver lts-9.18 script --package ghc --package text

{-# LANGUAGE OverloadedStrings #-}
import Data.Text (Text)
import qualified Data.Text as T
import Util (filterByList)
import Data.List

caught :: [Int] -> [Int] -> [Bool]
caught = zipWith (\d k -> d `rem` (2*(k-1)) == 0)

severity :: [Int] -> [Int] -> [Bool] -> Int
severity depths ranges catches =
  sum $ zipWith (*) depths' ranges'
  where depths' = filterByList catches depths
        ranges' = filterByList catches ranges

findDelay :: Int -> [Int] -> [Int] -> Int
findDelay dt depths ranges =
  if not . or $ caught (map (+dt) depths) ranges then
    dt
  else findDelay (dt+1) depths ranges

main :: IO ()
main = do
  contents <- T.lines . T.pack <$> getContents
  let depths:ranges:_ = transpose $ map (read . T.unpack) . T.splitOn ": " <$> contents
  putStrLn . show $ severity depths ranges (caught depths ranges)
  putStrLn . show $ findDelay 0 depths ranges
  
