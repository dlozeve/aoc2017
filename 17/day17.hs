#!/usr/bin/env stack
-- stack --resolver lts-9.18 script

{-# LANGUAGE BangPatterns #-}

import Data.List

rotate :: Int -> [a] -> [a]
rotate n xs = take (length xs) (drop n (cycle xs))

spinlock :: (Num a, Enum a) => a -> Int -> [a] -> [a]
spinlock n steps buf = foldl' f buf [1..n]
  where f !curbuf !i = let !newbuf = rotate (steps+1) curbuf in i:newbuf

valueAfter :: Eq t => t -> [t] -> t
valueAfter n buf = buf !! ((i + 1) `rem` length buf)
  where Just i = elemIndex n buf

main :: IO ()
main = do
  let input = 312
      n = 2017
      start = [0]
  print $ valueAfter n $ spinlock n input start
  -- let n = 50000000
  -- print $ valueAfter 0 $ spinlock n input start
