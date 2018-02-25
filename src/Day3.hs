module Day3
    (
    ) where

import Data.List(minimumBy, elemIndex)
import Data.Function(on, fix)

findCircleEnd :: Int -> Int
findCircleEnd 0 = 0
findCircleEnd n = head [ x*x | x <- [1,3..], x*x >= n ]

edges :: Int -> [Int]
edges n = take 4 $ n : go n where
  go m =  let m' = m - round (sqrt (fromIntegral n) - 1) in m' : go m'

closestEdge :: Int -> [Int] -> Int
closestEdge n = minimumBy (compare `on` (abs . (-) n))

stepsRequired :: Int -> Int
stepsRequired n
  | n `elem` edges (findCircleEnd n) = round $ sqrt (fromIntegral $ findCircleEnd n) - 1
  | otherwise = let pathFromEdge = abs $ n - closestEdge n (edges $ findCircleEnd n)
                    longestPath  = round . sqrt . fromIntegral $ findCircleEnd n
                in  longestPath - 1 - pathFromEdge

answer1 = stepsRequired 289326

corners :: Int -> [Int]
corners 1 = [1,1,1,1]
corners n = let
    ce = findCircleEnd n
    eL = pred . round . sqrt $ fromIntegral ce
    in [ce - (3*eL), ce - (2*eL), ce - eL, ce]

-- first in circle (2, 9, 26)
fstInCrcl :: Int -> Bool
fstInCrcl i = pred i `elem` take i [ x*x | x <- [1,3..] ]

-- last corner of the circle
lastCrnr :: Int -> Bool
lastCrnr i = elem i $ take i [ x*x | x <- [1,3..] ]

beforeLast :: Int -> Bool
beforeLast n = elem (succ n) $ take n [ x*x | x <- [1,3..] ]

crnr :: Int -> Bool
crnr i = i `elem` corners i

nxtAftrCrnr :: Int -> Bool
nxtAftrCrnr i = (i-1) `elem` corners i

nxtToCrnr :: Int -> Bool
nxtToCrnr i = (i+1) `elem` corners i

-- has not passed first corner yet
notP1stC :: Int -> Bool
notP1stC n = n < head (corners n)

lcocl :: Int -> Int
lcocl 1 = 1
lcocl n = n' * n' where n' = flip (-) 2 . round . sqrt . fromIntegral $ findCircleEnd n

-- WARNING: works only on corners
adjCrnr :: Int -> Int
adjCrnr 1 = 1
adjCrnr n = (!! i) $ corners k where
  Just i = elemIndex n $ corners n
  k      = lcocl n

-- index of last passed corner
pastCrnrI :: Int -> Int
pastCrnrI 1 = 1
pastCrnrI i = res where
  Just res      = elemIndex (go i (head $ corners i) (corners i)) (corners i)
  go _ r []     = r
  go i c (x:xs)
    | i > x     = go i x xs
    | otherwise = c

-- first corner on last circle
fstCrnOnLast :: Int -> Int
fstCrnOnLast n = head . corners $ lcocl n

-- last corner on last circle
lstCrnOnLast :: Int -> Int
lstCrnOnLast n = last . corners $ lcocl n

-- distance to 1st corner
distTo1C :: Int -> Int
distTo1C 1 = 1
distTo1C n = head (corners n) - n

--distance to previous corner
distToPrevC :: Int -> Int
distToPrevC n = n - n' where
  n' = (!!) (corners n) $ pastCrnrI n

lastPassedCrn :: Int -> Int
lastPassedCrn n = (!!) (corners n) (pastCrnrI n)

-- last passed corner on previous circle
lastPdOnPrv :: Int -> Int
lastPdOnPrv 1 = 1
lastPdOnPrv n = (!!) (corners $ fstCrnOnLast n) (pastCrnrI n)

memoize :: (Int -> Int) -> (Int -> Int)
memoize f = (map f [0 ..] !!)

solve :: Int -> Int
solve = fix (memoize . a2)

a2 :: (Int -> Int) -> Int -> Int
a2 f 1 = 1
a2 f 2 = 1
a2 f n
  | fstInCrcl n   = f (n-1) + f (succ . lcocl $ pred n)
  | lastCrnr n    = f (n-1) + f (lcocl n) + f (succ (lcocl n))
  | crnr n        = f (n-1) + f (adjCrnr n)
  | beforeLast n  =
    let
       sm = if n == 8
            then f 1 + f 2 + f (n-2)
            else f (lstCrnOnLast n) + f (pred $ lstCrnOnLast n) + f (succ $ lstCrnOnLast n)
       in f (n-1) + sm
  | nxtAftrCrnr n =
    let
       sm = if n > 9 then f (lcocl n) + f (succ $ lcocl n) else f (lcocl n)
       in f (n-1) + f (n-2) + sm
  | nxtToCrnr n   = f (n-1) + f (lcocl n) + f (pred $ lcocl n)
  | notP1stC n    =
    let
       d = distTo1C n
       crn = fstCrnOnLast n
       in f (crn - d) + f (crn - pred d) + f (crn - (d - 2))
  | otherwise     =
    let
      d = distToPrevC n
      crn = lastPdOnPrv n
      in f (crn - d) + f (crn - pred d) + f (crn - (d - 2))


