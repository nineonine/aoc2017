-- should be compiled - takes too much time if interpreted
module Day5
    (
      answer1
    , answer2
    ) where

data SZ a = SZ [a] [a] deriving (Show) -- Stack Zipper

getInput :: IO [Int]
getInput = (map read . lines) <$> readFile "input/day5.txt"

-- focus on first element
initZip :: [Int] -> SZ Int
initZip [] = SZ [] []
initZip (x:xs) = SZ [x] xs

left :: SZ Int -> Maybe (SZ Int)
left (SZ (x:xs) ys) = return $ SZ xs (x:ys)
left _ = Nothing

right :: SZ Int -> Maybe (SZ Int)
right (SZ xs (y:ys)) = return $ SZ (y:xs) ys
right _ = Nothing

-- not total
focus :: SZ Int -> Int
focus (SZ (x:_) _) = x

move :: Int -> SZ Int -> Maybe (SZ Int)
move = go where
  go 0 xs = Just xs
  go m xs
    | m > 0 = case right xs of
        Just xs' -> go (m-1) xs'
        Nothing  -> Nothing
    | otherwise = case left xs of
        Just xs' -> go (m+1) xs'
        Nothing  -> Nothing

applyToFocus :: (Int -> Int) -> SZ Int -> SZ Int
applyToFocus f (SZ (x:xs) ys) = SZ (f x : xs) ys

solve' :: (Int -> Int) -> [Int] -> Int
solve' _ []   = 0
solve' f l@(x:_) = go 0 x $ initZip l where
  -- go counter numOfSteps ziplist
  go c n zs = case move n (applyToFocus f zs) of
    Nothing  -> c + 1
    Just zs' -> go (c + 1) n' zs' where n' = focus zs'

answer1 = solve' succ <$> getInput

answer2 = solve' succIfBiggerThan3 <$> getInput where
  succIfBiggerThan3 i = if i >= 3 then pred i else succ i

