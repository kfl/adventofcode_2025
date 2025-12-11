{-# LANGUAGE LambdaCase, ViewPatterns #-}
module Main where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import qualified Data.Set as Set
import Data.Set (Set)

import qualified Data.Sequence as Seq
import Data.Sequence (Seq (..), (><))

import Data.Maybe (fromJust)
import qualified Data.Vector as V
import qualified GHC.Utils.Misc as Cheat

import qualified Data.Hashable as H
import qualified Data.OrdPSQ as Q
import qualified Data.HashMap.Strict as HMap



test =  map parse [ "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"
                  , "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}"
                  , "[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"
                  ]
input = map parse . lines <$> readFile "input.txt"

type Lights = [Bool]
type Button = [Int]
type Joltage = [Int]
type Machine = (Lights, [Button], Joltage)
type Input = [Machine]

parse :: String -> Machine
parse str = (map (== '#') $ L.init $ drop 1 lights, map readInts buttons, readInts joltage)
  where (lights : rest) = Cheat.split ' ' str
        (buttons, joltage) = fromJust $ L.unsnoc rest
        readInts (_ : s) = read $ "[" <> L.init s <> "]"

type Part1Machine = (V.Vector Bool, [Set Int])

convert :: [Machine] -> [Part1Machine]
convert machines = [ (V.fromList lights, map Set.fromList buttons)
                   | (lights, buttons, _) <- machines]

press lights button = V.imap (\i x -> if Set.member i button then not x else x) lights

type Distance = Int

bfs :: Ord a => (a -> [a]) -> a -> (a -> Bool) -> Maybe [(a, Distance)]
bfs next initial found = (`zip` [0..]) <$> go Set.empty (Seq.singleton [initial])
  where
    go _ Empty = Nothing
    go seen (path@(x:_) :<| queue)
      | found x = Just $ reverse path
      | x `Set.member` seen = go seen queue
      | otherwise = go (Set.insert x seen) (queue <> Seq.fromList [y : path | y <- next x])

part1 :: Input -> Int
part1 input = sum [length (solve m) - 1 | m <- ms ]
  where ms = convert input
        solve (goal, buttons) = fromJust $ bfs (\s -> map (press s) buttons) initial (== goal)
          where initial = V.replicate (V.length goal) False
answer1 = part1 <$> input

type Part2Machine = (V.Vector Int, [Set Int])

convert2 :: [Machine] -> [Part2Machine]
convert2 machines = [ (V.fromList joltage, map Set.fromList buttons)
                    | (_, buttons, joltage) <- machines]

press2 joltage button = V.imap (\i x -> if Set.member i button then x+1 else x) joltage

dijkstra :: (Num cost, Ord cost, Ord state)
         => (state -> [(state, cost)])
         -> (state -> Bool)
         -> state
         -> Maybe (cost, [state])
dijkstra next found initial = loop initPathCost initPrev startFrontier
  where
    x `less` may = maybe True (x <) may
    m !? state = Map.lookup state m

    update n c = Q.insert n c ()

    startFrontier = Q.singleton initial 0 ()
    initPathCost  = Map.singleton initial 0
    initPrev      = Map.empty

    loop _ _ (Q.minView -> Nothing) = Nothing
    loop pathCost prev (Q.minView -> Just(s, c, _, frontier))
      | found s = Just (c, reconstruct s prev)
      | otherwise = loop pathCost' prev' frontier'
      where
        relevant = [ (n, cc) | (n, sc) <- next s,
                               let cc = c + sc,
                               cc `less` (pathCost !? n) ]
        (frontier', pathCost', prev') = L.foldr updateAll (frontier, pathCost, prev) relevant
        updateAll (n, cc) (front, pathC, prev) =
              (update n cc front, Map.insert n cc pathC, Map.insert n s prev)

    reconstruct goal prev = reverse $ go [] goal
      where go acc cur = maybe (cur:acc) (go (cur:acc)) $ Map.lookup cur prev




part2 :: Input -> Int
part2 input = sum [length (solve m) - 1 | m <- ms ]
  where ms = convert2 input
        solve (goal, buttons) = snd $ fromJust $ dijkstra next (== goal) initial
          where initial = V.replicate (V.length goal) 0
                valid vec = and $ V.zipWith (<=) vec goal
                next s = [ (s', 1) | b <- buttons
                                   , let s' = press2 s b
                                   , valid s']

answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  putStrLn "Currently part2 will just spin the wheels until the OOM kills it, might as well stop it now"
  print $ part2 inp
