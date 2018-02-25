{-# LANGUAGE BangPatterns #-}

module Day13
    (
      answer1
    , answer2
    ) where

type Depth     = Int -- index
type Range     = Int
type Delay     = Int
data Layer     = L !Depth !Range deriving (Show)
type Firewall  = [Layer]

getInput :: IO String
getInput = readFile "input/day13.txt"

parse :: String -> [Layer]
parse = fmap (parseLine . words) . lines where
  parseLine = L <$> depth <*> range where
    depth = read . takeWhile (/= ':') . head
    range = read . (!! 1)

solve1 :: Firewall -> Int
solve1 = severity . go [] where
  severity = sum . fmap (uncurry (*))
  go rs [] = rs
  go rs (l@(L d r):xs)
     | detected' 0 l = go ((d,r):rs) xs
     | otherwise = go rs xs

{-# INLINE detected' #-}
detected' :: Delay -> Layer -> Bool
detected' dl (L d r) = (d + dl) `mod` (2 * (r - 1)) == 0

detectedSmWhere :: Delay -> Firewall -> Bool
detectedSmWhere d = any (detected' d)

solve2 :: Firewall -> Int
solve2 = go 1 where
  go !n fw
    | not $ detectedSmWhere n fw = n
    | otherwise                  = go (succ n) fw


answer1 :: IO Int
answer1 = (solve1 . parse) <$> getInput

answer2 :: IO Int
answer2 = (solve2 . parse) <$> getInput

{-

My first horrible brute-force solution
took up to 60 seconds compiled with -02
had to look up smart maths
ideas come from Chinese Remainder Theorem

-}