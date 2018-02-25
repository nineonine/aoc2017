{-# LANGUAGE BangPatterns #-}

module Day6
    ( answer1
    ) where

import Data.List (sortBy)
import Data.Function (on)

type Index = Int
type Bank = (Index, Int)
type BankHistory = [[Bank]]

i1 = zip [1..] [10,3,15,10,5,15,5,15,9,2,5,8,5,2,3,6] :: [Bank]

max' :: [Bank] -> [Bank]
max' []     = []
max' (x:xs) = go x xs where
  go !y []      = [y]
  go !y (y':ys)
    | snd y < snd y' = go y' ys
    | otherwise      = go y  ys

flushBank :: Index -> [Bank] -> [Bank]
flushBank _ [] = []
flushBank i xs = go i xs where
  go i (y:ys) -- i > y throws Exception
    | i == fst y = (i, 0) : ys
    | otherwise  = y : go i ys

redistribute :: Index -> Int -> [Bank] -> [Bank]
redistribute _ _ [] = []
redistribute i n xs = sort' $ fR i r $ eR q xs where -- fillRem i rem' $ fillEvenly quot' xs where
  (!q, !r) = quotRem' n xs
  sort' = sortBy (compare `on` fst)

  quotRem' :: Int -> [Bank] -> (Int, Int)
  quotRem' n bs = quotRem n (length bs)

  eR :: Int -> [Bank] -> [Bank]
  eR 0 zs = zs
  eR k zs = fmap (fmap (k+)) zs

  fR :: Index -> Int -> [Bank] -> [Bank]
  fR _ 0 xs = xs
  fR i r xs = sort' $ take (length xs) $ succ' r $ tail $ dropWhile (\x -> i > fst x ) $ cycle xs where
    sort' = sortBy (compare `on` fst)
    succ' 0 xs     = xs
    succ' n (x:xs) = fmap succ x : succ' (n-1) xs


tick :: [Bank] -> [Bank]
tick [] = []
tick xs = result where
  [(!i, !blox)] = max' xs
  !flushed     = flushBank i xs
  !result      = redistribute i blox flushed

occuredBefore :: [Bank] -> BankHistory -> Bool
occuredBefore = go where
  go bs = foldr (\ bs' -> (||) (bs == bs')) False

run :: [Bank] -> Int
run [] = 0
run xs = go 1 [xs] xs where
  go !n history ys =
    let !ys' = tick ys
    in if occuredBefore ys' history
       then n
       else go (n+1) (ys':history) ys'

run2 :: [Bank] -> Int
run2 [] = 0
run2 xs = go 1 [xs] xs where
 go !n history ys =
   let !ys' = tick ys
   in if occuredBefore ys' history
      then measureCycle ys' history
      else go (n+1) (ys':history) ys'

measureCycle :: [Bank] -> BankHistory -> Int
measureCycle b bh = go b bh 0 where
  go _ [] n = n
  go b (b':bs) n
    | b == b' = n + 1
    | otherwise = go b bs (n+1)


{-

Terrible performance here (up to 12 seconds)
lots of intermediary data structures : (:) and (,)
Vector/Array would give us the desired performance
sticked to list here just because we can 8D !

-}

answer1 = run i1

answer2 = run2 i1
