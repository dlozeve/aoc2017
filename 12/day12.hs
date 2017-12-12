#!/usr/bin/env stack
-- stack --resolver lts-9.18 script

{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec.Text
import Data.Text (Text)
import qualified Data.Text as T
import Control.Applicative
import Data.List

parseAdjacencyList :: Parser (Int, [Int])
parseAdjacencyList = do
  node <- decimal
  skipSpace
  string "<->"
  skipSpace
  neighbours <- many1 (option "" (string ", ") *> decimal)
  return (node, neighbours)

dfs :: [(Int, [Int])] -> [Int] -> Int -> [Int]
dfs graph visited vertex =
  case lookup vertex graph of
    Nothing -> vertex : visited
    Just neighbours -> concat $ map f neighbours
      where f n = if n `elem` visited then visited
                  else dfs graph (neighbours ++ visited) n

main :: IO ()
main  = do
  contents <- T.lines . T.pack <$> getContents
  let Right graph = sequence $ parseOnly parseAdjacencyList <$> contents
  -- mapM_ (putStrLn . show) graph
  let cc0 = nub $ dfs graph [] 0
  -- putStrLn . show $ cc0
  putStrLn . show . length $ cc0
  let ccs = nub $ map (sort . nub . dfs graph []) (map fst graph)
  -- putStrLn . show $ ccs
  putStrLn . show . length $ ccs
