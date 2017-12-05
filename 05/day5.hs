#!/usr/bin/env stack
-- stack --resolver lts-9.14 script

jump :: Int -> [Int] -> [Int] -> Maybe ([Int], [Int])
jump n back front =
  if n == 0 then
    Just (back, front)
  else if n > 0 then
    case front of
      [] -> Nothing
      x:xs -> jump (n-1) (x:back) xs
  else
    case back of
      [] -> Nothing
      x:xs -> jump (n+1) xs (x:front)

countJumps :: Int -> [Int] -> [Int] -> Int
countJumps count back [] = count
countJumps count back (x:xs) =
  case jump x back ((x+1):xs) of
    Nothing -> count + 1
    Just (back', front') -> countJumps (count+1) back' front'

countJumps2 :: Int -> [Int] -> [Int] -> Int
countJumps2 count back [] = count
countJumps2 count back (x:xs) =
  case jump x back (newx:xs) of
    Nothing -> count + 1
    Just (back', front') -> countJumps2 (count+1) back' front'
  where newx = if x >= 3 then x-1 else x+1


main :: IO ()
main = do
  front <- map read <$> lines <$> getContents
  putStrLn . show $ countJumps2 0 [] front
