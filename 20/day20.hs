#!/usr/bin/env stack
-- stack --resolver lts-9.20 script

{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec.Text hiding (take)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (minimumBy, (\\))
import Data.Function (on)

data Point a =
  Point { position :: (a,a,a)
        , velocity :: (a,a,a)
        , acceleration :: (a,a,a)
        }
  deriving (Show, Eq)

parseVector :: Text -> Parser (Int,Int,Int)
parseVector id = do
  string id
  string "=<"
  x <- signed decimal
  string ","
  y <- signed decimal
  string ","
  z <- signed decimal
  string ">"
  return (x,y,z)

parsePoint :: Parser (Point Int)
parsePoint = do
  pos <- parseVector "p"
  string ","
  skipSpace
  vel <- parseVector "v"
  string ","
  skipSpace
  acc <- parseVector "a"
  return $ Point pos vel acc

update :: Num a => Point a -> Point a
update (Point (posx,posy,posz) (velx,vely,velz) (accx,accy,accz)) =
  Point (posx',posy',posz') (velx',vely',velz') (accx,accy,accz)
  where velx' = velx + accx
        vely' = vely + accy
        velz' = velz + accz
        posx' = posx + velx'
        posy' = posy + vely'
        posz' = posz + velz'

removeCollisions :: Eq a => [Point a] -> [Point a]
removeCollisions [] = []
removeCollisions (p:ps) = case filter (\x -> position x == position p) ps of
  [] -> p : removeCollisions ps
  xs -> removeCollisions $ ps \\ xs

distance :: Num a => Point a -> a
distance (Point (x,y,z) _ _) = abs x + abs y + abs z

closestZero :: [Point Int] -> Integer
closestZero = fst . minimumBy (compare `on` (distance . snd)) . zip [0..]

main :: IO ()
main = do
  contents <- T.lines . T.pack <$> getContents
  let Right points = mapM (parseOnly parsePoint) contents
  -- mapM_ print points
  print . take 1000 . map closestZero $ iterate (map update) points
  print . take 200 . map length $ iterate (removeCollisions . map update) points
