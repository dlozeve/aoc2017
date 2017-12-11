#!/usr/bin/env stack
-- stack --resolver lts-9.17 script

{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec.Text
import Data.Text (Text)
import qualified Data.Text as T
import Control.Applicative
import Data.List

data Direction = N | NE | NW | S | SE | SW
  deriving (Eq, Show)

type Path = [Direction]

type Coord = (Int, Int, Int)

parseDirection :: Parser Direction
parseDirection =
  (string "ne" >> return NE)
  <|> (string "nw" >> return NW)
  <|> (string "se" >> return SE)
  <|> (string "sw" >> return SW)
  <|> (string "n" >> return N)
  <|> (string "s" >> return S)

parsePath :: Parser Path
parsePath = do
  path <- many' (option "" (string ",") *> parseDirection)
  return path

step :: Coord -> Direction -> Coord
step (x,y,z) dir = case dir of
  N -> (x, y+1, z-1)
  NE -> (x+1, y, z-1)
  NW -> (x-1, y+1, z)
  S -> (x, y-1, z+1)
  SE -> (x+1, y-1, z)
  SW -> (x-1, y, z+1)

finalCoord :: Path -> Coord
finalCoord = foldl' step (0,0,0)

allSteps :: Path -> [Coord]
allSteps = scanl' step (0,0,0)

distance :: Coord -> Int
distance (x,y,z) = maximum $ map abs [x,y,z]

main :: IO ()
main = do
  Right path <- parseOnly parsePath . T.pack <$> getLine
  --putStrLn . show $ path
  putStrLn . show . distance .finalCoord $ path
  putStrLn . show . maximum . map distance . allSteps $ path
