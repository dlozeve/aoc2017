#!/usr/bin/env stack
-- stack --resolver lts-9.17 script

{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec.Text
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Control.Applicative
import Data.List

type Register = Text

data Order = L | LEQ | EEQ | GEQ | G | NEQ
  deriving (Show, Eq)

data Operation = Inc | Dec
  deriving (Show, Eq)

data Instruction = Instruction
  { register :: Register
  , op :: Operation
  , value :: Int
  , condition :: (Register, Order, Int)
  } deriving (Show, Eq)

parseOperation :: Parser Operation
parseOperation =
  (string "inc" >> return Inc) <|> (string "dec" >> return Dec)

parseOrder :: Parser Order
parseOrder =
  (string ">=" >> return GEQ)
  <|> (string ">" >> return G)
  <|> (string "<=" >> return LEQ)
  <|> (string "<" >> return L)
  <|> (string "==" >> return EEQ)
  <|> (string "!=" >> return NEQ)

parseInstruction :: Parser Instruction
parseInstruction = do
  reg <- many1 letter
  skipSpace
  op <- parseOperation
  skipSpace
  val <- signed decimal
  skipSpace
  string "if"
  skipSpace
  condReg <- many1 letter
  skipSpace
  condOrd <- parseOrder
  skipSpace
  condVal <- signed decimal
  return $ Instruction (T.pack reg) op val ((T.pack condReg), condOrd, condVal)

executeInstruction :: Map.Map Register Int -> Instruction -> Map.Map Register Int
executeInstruction map i =
  if cond then
    case op i of
      Inc -> Map.insertWith (+) (register i) (value i) map
      Dec -> Map.insertWith (+) (register i) (-(value i)) map
  else map
  where (condReg, condOrd, condVal) = condition i
        curCondVal = Map.findWithDefault 0 condReg map
        cond = case condOrd of
                 L -> curCondVal < condVal
                 LEQ -> curCondVal <= condVal
                 EEQ -> curCondVal == condVal
                 GEQ -> curCondVal >= condVal
                 G -> curCondVal > condVal
                 NEQ -> curCondVal /= condVal

executeInstructions :: [Instruction] -> [Map.Map Register Int]
executeInstructions = scanl' executeInstruction Map.empty 

main :: IO ()
main = do
  contents <- T.lines . T.pack <$> getContents
  let Right instructions = sequence $ parseOnly parseInstruction <$> contents
  --mapM_ (putStrLn . show) instructions
  let maps = executeInstructions instructions
  putStrLn . show . maximum . Map.elems . last $ maps
  putStrLn . show . maximum . concat $ Map.elems <$> maps
