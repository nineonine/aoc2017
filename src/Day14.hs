module Day14
    (
    answer1
    ) where

import Day10 (mkHash)

import qualified Data.ByteString.Lazy as L
import           Data.ByteString.Builder

import qualified Data.Vector.Unboxed as V
import           Data.List (foldl')
import           Data.Monoid

import Numeric (readHex, showIntAtBase)
import Data.Char (intToDigit)

type Hash = String
type Grid = [Hash]
type VHash = V.Vector Int

input :: String
input = "ffayrhll"

mkGrid :: String -> Grid
mkGrid = go 0 where
  go 128 s = []
  go n   s = let s' = (s++ ('-':show n)) in mkHash s' : go (succ n) s

countOnes :: Hash -> Int
countOnes = foldl' step 0 where
  step acc a = (+) acc $ count' a where
    count' = length . filter (== '1') . toHexDigits

toHexDigits :: Char -> String
toHexDigits c =
  let [(num,_)] = readHex [c]
  in showIntAtBase 2 intToDigit num ""

chrTo4b :: Char -> String
chrTo4b = to4Bytes . toHexDigits where
  to4Bytes s = case length s of
    4 -> s
    _ -> replicate (4 - length s) '0' ++ s

-- grpAdj :: Hash -> [[Int]]
-- grpAdj = go [] 0 where
--   go gs [] _ = gs


solve1 :: Grid -> Int
solve1 = foldl' (+) 0 . map countOnes

answer1 = solve1 (mkGrid input)
