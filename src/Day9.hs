module Day9
    (
    ) where

type Score = Int
type Garbage = Int

getInput :: IO String
getInput = readFile "input/day9.txt"

answer :: String -> (Score, Garbage) -- (total group score, total garbage)
answer ss = go ss 0 0 0 where
  go [] _ t g       = (t, g)
  go ('{':xs) p t g = go xs (succ p) (succ p + t) g
  go ('}':xs) p t g = go xs (pred p) t g
  go (',':xs) p t g = go xs p t g
  go ('<':xs) p t g = let (xs',n) = dropGarbage xs in go xs' p t (g+n)

dropGarbage :: String -> (String, Int)
dropGarbage s  = go s 0 where
  go [] _      = ([], 0)
  go ('!':xs) n = go (tail xs) n
  go ('>':xs) n = (xs, n)
  go (_:xs)   n = go xs (succ n)
