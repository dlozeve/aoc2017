#!/usr/bin/env stack
-- stack --resolver lts-9.14 script

{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec.Text
import Data.Text (Text)
import qualified Data.Text as T
import Data.List
import Data.Maybe

data Program = Program
  { programName :: Text
  , programWeight :: Int
  , programChildren :: [Text]
  } deriving (Show, Eq)

parseProgram :: Parser Program
parseProgram = do
  name <- many1 letter
  skipSpace
  char '('
  weight <- decimal
  char ')'
  option "" $ count 4 anyChar
  children <- many' (option "" (string ", ") *> many1 letter)
  return $ Program (T.pack name) weight (map T.pack children)

findRoots :: [Program] -> [Program]
findRoots xs =
  filter (\x -> programName x `notElem` l) xs
  where l = concat $ map programChildren xs

totalWeight :: [Program] -> Program -> Int
totalWeight _ (Program _ w []) = w
totalWeight xs (Program _ w cs) =
  w + (sum $ totalWeight xs <$> (filter (\x -> programName x `elem` cs) xs))

childrenWeights :: [Program] -> Program -> [Int]
childrenWeights xs (Program _ _ children) =
  totalWeight xs <$> (filter (\x -> programName x `elem` children) xs)

isUnbalanced :: [Int] -> Bool
isUnbalanced [] = False
isUnbalanced weights =
  not . and $ map (== head weights) (tail weights)

main :: IO ()
main = do
  contents <- T.lines . T.pack <$> getContents
  let programs = map (parseOnly parseProgram) contents
  --mapM_ (mapM_ (putStrLn . show)) $ programs
  putStrLn "Root: "
  mapM_ (mapM_ (putStrLn . show)) $ findRoots <$> sequence programs
  putStrLn "Unbalanced: "
  let Right programs' = sequence programs
  let weights = zip programs' $ childrenWeights programs' <$> programs'
  let unbalanced = filter (isUnbalanced . snd) weights
  mapM_ (putStrLn . show) unbalanced
  
