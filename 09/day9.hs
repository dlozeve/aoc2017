#!/usr/bin/env stack
-- stack --resolver lts-9.17 script

import Data.List

data Stream = Stream
  { depth :: Int
  , score :: Int
  , ignoreNextChar :: Bool
  , garbage :: Bool
  , countGarbage :: Int
  } deriving (Show, Eq)

processChar :: Stream -> Char -> Stream
processChar stream c =
  if ignoreNextChar stream then
    stream { ignoreNextChar = False }
  else if garbage stream then
    case c of
      '!' -> stream { ignoreNextChar = True }
      '>' -> stream { garbage = False }
      _ -> stream { countGarbage = countGarbage stream + 1 }
  else if c == '}' then
    stream { depth = depth stream - 1, score = score stream + depth stream }
  else if c == '{' then
    stream { depth = depth stream + 1 }
  else if c == '<' then
    stream { garbage = True }
  else stream

processStream :: String -> Stream
processStream = foldl' processChar (Stream 0 0 False False 0)

main :: IO ()
main = do
  stream <- processStream <$> getContents
  putStrLn . show $ score stream
  putStrLn . show $ countGarbage stream
