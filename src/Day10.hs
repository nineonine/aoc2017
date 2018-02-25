module Day10
    (
    mkHash
    ) where

import Debug.Trace (trace)
import Data.Char
import Data.Bits (xor)
import Numeric (showHex)

type Position = Int
type SkipSize = Int
type Length   = Int

maxOfEl :: Int
maxOfEl = 256

input :: [Int]
input = [31,2,85,1,80,109,35,63,98,255,0,13,105,254,128,33]

initList :: [Int]
initList = take maxOfEl [0..]

splitter :: [Int] -> Position -> Length -> ([Int],[Int]) -- (reversed, untouched)
splitter [] _ _ = ([],[])
splitter xs 0 l = (\(a,b) -> (reverse a, b)) $ splitAt l xs
splitter xs p l = go (drop p $ cycle xs) [] l where
  go xs     ys 0 = (ys, take (maxOfEl - length ys) xs)
  go (x:xs) ys l = go xs (x:ys) (pred l)

appendBack :: Position -> ([Int], [Int]) -> [Int]
appendBack 0 (xs,ys) = xs ++ ys
appendBack n (xs,ys) = take maxOfEl $ drop (maxOfEl - n) $ cycle (xs ++ ys)

-- next Position
nP :: Position -> SkipSize -> Length -> Position
nP p s l = head . drop (p+s+l) $ cycle [0..maxOfEl - 1]

round' :: [Int] -> [Length] -> Position -> SkipSize -> ([Int], Position, SkipSize)
round' list input m n = go list m n input where -- go list currentPos skipSize puzzleInput
  go l c s []     = (l, c, s)
  go l c s (i:is) =
    let
      l' = appendBack c $ splitter l c i
      c' = nP c s i
      s' = succ s
      -- lg = "Position: " ++ (show c) ++ " | skipSize: " ++ (show s) ++ " | length: " ++(show i) ++ " | list: " ++ (show l) ++ " | updatedList: " ++ (show l')
      in go l' c' s' is

answer1 =
  let (l, _, _) = round' initList input 0 0
  in product $ take 2 l

round64 :: [Int] -> [Int]
round64 input = go 63 l' p s where
  (l', p, s) = round' initList input 0 0
  go 0 l _ _ = l
  go n l p s =
    let
      (l', p', s') = round' l input p s
      in go (pred n) l' p' s'

mkSparseHash :: [Int] -> [Int]
mkSparseHash = round64

mkDenseHash :: [Int] -> [Int]
mkDenseHash = go where
  go [] = []
  go xs = let (fs, ls) = splitAt 16 xs in foldl1 xor fs : go ls

prepareInput :: String -> [Int]
prepareInput s = toBytes ++ seqToAdd where
  toBytes = map ord s
  seqToAdd = [17, 31, 73, 47, 23]


answer2 :: String
answer2 = mkHash "31,2,85,1,80,109,35,63,98,255,0,13,105,254,128,33"

mkHash :: String -> String
mkHash s =
  let
    toHexS i = showHex i ""
    f i = case length $ toHexS i of
      1 -> '0':toHexS i
      _ -> toHexS i
    denseHash = mkDenseHash . mkSparseHash $ prepareInput s
    in concatMap f denseHash



