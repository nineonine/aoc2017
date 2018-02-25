module Day12
    (
    ) where

import qualified Data.List as L
import qualified Data.IntMap.Lazy as IM

import Debug.Trace (trace)

getInput :: IO String
getInput = readFile "input/day12.txt"

type PipeId     = Int
type PipeMap    = IM.IntMap [PipeId]
type PipeConn   = (PipeId, [PipeId])
data PipeSystem = SelfConnected PipeId
                | PipeId :<->: [PipeSystem] deriving Show

maxPipe :: PipeId
maxPipe = 2000

parseConn :: String -> PipeConn
parseConn = (,) <$> pipe <*> pipes where
  removeComma = filter (/= ',')
  pipe  = read . head . words
  pipes = map (read . removeComma) . drop 2 . words

parse :: String -> [PipeConn]
parse = map parseConn . lines

mkCnMap :: [PipeConn] -> PipeMap
mkCnMap = L.foldl' insertConn IM.empty where
  insertConn mp (p,ps) = IM.insert p ps mp

-- build pipe system
build :: PipeMap -> PipeId -> PipeSystem
build pm pid = go [] pid (pm IM.! pid) where
  go st n xs
    | n == head xs && length xs == 1 = SelfConnected n
    | otherwise = n :<->: map f diff where
      diff = xs L.\\ st
      unn' = L.union [n] $ L.union st diff
      f a  = go unn' a (pm IM.! a)

length' :: PipeSystem -> Int
length' (SelfConnected _) = 1
length' ps = go ps where
  go (SelfConnected _) = 0
  go (_ :<->: ps)      = 1 + sum (map go ps)

psToList :: PipeSystem -> [Int]
psToList = L.sort . go where
  go (SelfConnected x) = [x]
  go (p :<->: ps)      = p : concatMap go ps

input :: IO PipeMap
input = (mkCnMap . parse) <$> getInput

groups' :: PipeMap -> [PipeSystem]
groups' = go (L.take maxPipe [0..]) [] where
  go []     grps _  = grps
  go (x:xs) grps mp =
    let grp = build mp x
        lst = psToList grp
    in  go (xs L.\\ lst) (grp:grps) mp

answer1 :: IO Int
answer1 = do
  i <- input
  return . length' $ build i 0

answer2 :: IO Int
answer2 = do
  i <- input
  return . L.length $ groups' i
