module Day8
    (
    ) where

import Data.List
import qualified Data.Map.Strict as M

type RName        = String -- register name
type Registers    = M.Map RName Int
type Operation    = String
data Instruction  = I RName Operation Int Condition deriving (Show)
newtype Condition = Cond { check :: Registers -> Bool }

instance Show Condition where
  show _ = "Some Condition"

getInput :: IO [String]
getInput = lines <$> readFile "input/day8.txt"

parseInstruction :: String -> Instruction
parseInstruction s = I rname1 op i cond where
  [rname1, op, n1, if', rname2, rel, n2] = words s
  i    = read n1
  cond = Cond $ \m -> parseRelOp rel (m M.! rname2) (read n2)

parse :: [String] -> (Registers, [Instruction])
parse ss = (regs, is) where
  regs = foldl' (\a x -> M.insert x 0 a ) M.empty . nub $ map (head . words) ss
  is   = map parseInstruction ss

--relation operation parser
parseRelOp :: String -> (Int -> Int -> Bool)
parseRelOp "!=" = (/=)
parseRelOp "==" = (==)
parseRelOp ">"  = (>)
parseRelOp "<"  = (<)
parseRelOp ">=" = (>=)
parseRelOp "<=" = (<=)

oper :: Operation -> (Int -> Int -> Int)
oper "inc" i = flip (+) i
oper _     i = flip (-) i

solve1 :: Registers -> [Instruction] -> (Int,Int) -- (first part, second part)
solve1 regs is = (x, maxReg) where
  maxReg = maximum . map snd $ M.toList regs'
  (x,regs') = foldl' step (0,regs) is
  step (m,rs) (I n op i cond)
    | check cond rs =
      let
        regVal = (oper op i $ rs M.! n)
        m'= if regVal > m then regVal else m
      in (m', M.adjust (oper op i) n rs)
    | otherwise = (m,rs)

answer = do
  input <- getInput
  let (rs, is) = parse input
  return $ solve1 rs is