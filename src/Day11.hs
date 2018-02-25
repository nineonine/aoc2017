module Day11
    (
    ) where

import Data.List (foldl')

getInput :: IO String
getInput = readFile "input/day11.txt"

parse :: String -> [Direction]
parse = go where
  go []       = []
  go (',':xs) = go xs
  go xs       = let (dir,rest) = span (',' /=) xs in dir : go rest

type Cell = (Int, Int, Int) -- (x, y, z)
type Direction = String

fstCell :: Cell
fstCell = (0,0,0)

step :: Cell -> Direction -> Cell
step (x,y,z) "n"  = (x,succ y,pred z)
step (x,y,z) "ne" = (succ x,y,pred z)
step (x,y,z) "se" = (succ x,pred y,z)
step (x,y,z) "s"  = (x,pred y,succ z)
step (x,y,z) "sw" = (pred x,y,succ z)
step (x,y,z) "nw" = (pred x,succ y,z)

-- step with furthest position
stepWPos :: (Int, Cell) -> Direction -> (Int, Cell)
stepWPos (n, c) dir =
  let
    c' = step c dir
    d  = distance c'
    n' = if n < d then d else n
    in (n', c')

lastCell :: [Direction] -> Cell
lastCell = foldl' step fstCell

--furthest distance
fDist :: [Direction] -> Int
fDist = fst . foldl' stepWPos (0,fstCell)

distance :: Cell -> Int
distance (x,y,z) = (abs x + abs y + abs z) `div` 2

answer1 :: IO Int
answer1 = (distance . lastCell . parse) <$> getInput

answer2 :: IO Int
answer2 = (fDist . parse) <$> getInput