module Day4
    (
    ) where

import Data.List (sort)

getInput :: IO [[String]]
getInput = (map words . lines) <$> readFile "input/day4.txt"

isSet :: [String] -> Bool
isSet []     = True
isSet (x:xs) = go x xs where
  go _ []     = True
  go v (y:ys)
    | v == y    = False
    | otherwise = go v ys && go y ys


answer1 = fmap (length . filterFun) getInput where
  filterFun = filter isSet


answer2 = fmap (length . filterFun) getInput where
  filterFun = filter $ isSet . fmap sort