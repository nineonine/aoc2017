module Day2
    (
    ) where

import Data.Maybe(fromMaybe, catMaybes)
import Data.Foldable(foldl')
import Data.Traversable(traverse)

getInput :: IO [[String]]
getInput = (map words . lines) <$> readFile "input/day1.txt"

parse :: [[String]] -> [[Int]]
parse = fmap $ fmap read

solve :: ((Int, Int) -> Int) -> ([Int] -> Maybe (Int, Int)) -> [[Int]] -> Int
solve binOp pairGen ss = foldl' (+) 0 values where
  values = map binOp . catMaybes $ map pairGen ss

solve1 :: [[Int]] -> Int
solve1 = solve diff minmax where
  diff (a,b) = b - a

solve2 :: [[Int]] -> Int
solve2 = solve divResult evenlyDivide where
  divResult (a,b) = b `div` a

minmax :: [Int] -> Maybe (Int, Int)
minmax []     = Nothing
minmax (y:ys) = go ys (y,y) where
  go [] r = Just r
  go (x:xs) (i,a)
    | x < i = go xs (x,a)
    | x > a = go xs (i,x)
    | otherwise = go xs (i,a)

evenlyDivide :: [Int] -> Maybe (Int, Int)
evenlyDivide [] = Nothing
evenlyDivide (y:ys) = go y ys where
  go x [] = Nothing
  go x (x':xs)
    | max x x' `mod` min x x' == 0 = Just (min x x', max x x')
    | otherwise = case go x xs of
        Nothing -> go x' xs
        Just v  -> Just v


answer1 = (solve1 . parse) <$> getInput
answer2 = (solve2 . parse) <$> getInput
