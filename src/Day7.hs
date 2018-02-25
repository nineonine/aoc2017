module Day7
    (
    ) where

import qualified Data.List as L
import qualified Data.Map.Strict as M

getInput :: IO String
getInput = readFile "input/day7.txt"

type Name = String
type Weight = Int
type Program = (Name, Weight, [Name])
data ProgramTower = PT Name Weight [ProgramTower] deriving Show

name :: Program -> Name
name (n, _, _) = n

weight :: Program -> Weight
weight (_, w, _) = w

disc :: Program -> [Name]
disc (_, _, ns) = ns

mkProgramMap :: [Program] -> M.Map Name [Name]
mkProgramMap [] = M.empty
mkProgramMap xs = L.foldl' ( \a (n,_,ns)-> M.insert n ns a ) M.empty xs

-- was tooo lazy to use 1 map
mkMapWithWeights :: [Program] -> M.Map Name (Weight, [Name])
mkMapWithWeights [] = M.empty
mkMapWithWeights xs = L.foldl' ( \a (n,w,ns)-> M.insert n (w,ns) a ) M.empty xs

weightP :: [String] -> Int
weightP s = let c = tail . takeWhile (')'/=) . head $ tail s in read c

discP :: [String] -> [String]
discP s = let c = dropWhile (/= "->") s
         in case c of
           [] -> []
           _  -> map stripComma $ tail c where stripComma = takeWhile (/= ',')

parseProgram :: [String] -> Program
parseProgram = (,,) <$> head <*> weightP <*> discP

parse :: String -> [Program]
parse = fmap (parseProgram . words) . lines


programDepth :: M.Map Name [Name] -> Name -> Int
programDepth mp pname = go mp pname where
  go mnn n
    | M.member n mnn = let ps = mnn M.! n in succ . L.maximum $ map (go mnn) ps
    | otherwise     = 0


answer1 :: [Program] -> Maybe Program
answer1 []     = Nothing
answer1 (y:ys) = Just $ go pMap y (programDepth pMap $ name y) ys where

  pMap = mkProgramMap withoutEmpty

  withoutEmpty = L.filter ( \(_,_,d) -> case d of [] -> False; _ -> True ) ys -- :: [Program]

  go mp v _ []     = v
  go mp v d (x:xs) =
    let
       xDepth = programDepth mp $ name x
       xs' = L.filter ( \(n,_,_) -> L.notElem n $ disc x) xs
    in
      if xDepth > d
         then go mp x xDepth xs'
         else go mp v d      xs'


totalWeight :: M.Map Name (Weight, [Name]) -> Name -> Weight
totalWeight mp n =
  let
    (w, ns) = mp M.! n
    ws = sum $ map (totalWeight mp) ns
  in
    w + ws


goodWeight :: [Int] -> Int
goodWeight [] = 0
goodWeight is = head $ L.filter (not . onlyOnce is) is where
  onlyOnce [] v = False
  onlyOnce ys v = times ys v == 1 where
    times ys v = sum $ map (\a -> if a==v then 1 else 0) ys

-- ifnd disbalanced tree
findDisb :: [(Name, Weight)] -> Name
findDisb []  = []
findDisb nws = fst . head $ L.sortOn (timesOccured . snd) nws where
  timesOccured x = length $ L.elemIndices x (map snd nws)

-- ans: 1458
answer2 :: [Program] -> Weight
answer2 [] = 0
answer2 ps  =
  let
     Just (n,w,ns) = answer1 ps
  in go (mkMapWithWeights ps) 0 ns

  where
    go mp gW [] = 0
    go mp gW ps =
      let
        ws = L.map (totalWeight mp) ps
      in
        case L.length $ L.nub ws of
          1 -> gW - sum ws
          _ -> let
                  pn  = findDisb (zip ps ws)
                  ps' = snd $ mp M.! pn
               in go mp (goodWeight ws) ps'