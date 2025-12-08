{-# LANGUAGE LambdaCase #-}
module Main where

import qualified Data.List as L

import Data.Ord (Down(..))
import Data.Function (on)
import qualified Control.Monad.ST as ST
import Data.STRef (newSTRef)
import Control.Monad (forM, forM_, filterM)
import qualified UnionFind as UF


test =  map parse [ "162,817,812"
                  , "57,618,57"
                  , "906,360,560"
                  , "592,479,940"
                  , "352,342,300"
                  , "466,668,158"
                  , "542,29,236"
                  , "431,825,988"
                  , "739,650,466"
                  , "52,470,668"
                  , "216,146,977"
                  , "819,987,18"
                  , "117,168,530"
                  , "805,96,715"
                  , "346,949,466"
                  , "970,615,88"
                  , "941,993,340"
                  , "862,61,35"
                  , "984,92,344"
                  , "425,690,689"
                  ]
input = map parse . lines <$> readFile "input.txt"

type Pos = (Int, Int, Int)
type Input = [Pos]

parse :: String -> Pos
parse str = read $ "(" <> str <> ")"

allPairs :: [a] -> [(a,a)]
allPairs xs = [ (x, y) | (x:ys) <- L.tails xs, y <- ys ]

-- We don't need to use actual straight-line distance, just something
-- that is order preserving, thus we can use the following distance function
distance ((x1, y1, z1), (x2, y2, z2)) = (x1 - x2)^2 + (y1 - y2)^2 + (z1 - z2)^2

connect n input = ST.runST $ do
  counter <- newSTRef 0
  pointSets <- traverse (\p -> (,) p <$> UF.newSet counter) input

  let dists = L.sortOn snd [ ((s1, s2), distance (p1, p2))
                           | ((p1, s1), (p2, s2)) <- allPairs pointSets ]

  forM_ (take n dists) $ \((s1, s2), _) -> UF.union s1 s2

  roots <- forM (map snd pointSets) $ \s ->
    (,) <$> UF.findRoot s <*> UF.size s

  return $ L.sortOn Down $ map snd $ L.nubBy ((==) `on` fst) roots

part1 :: Input -> Int
part1 input = product $ take 3 $ connect 1000 input
answer1 = part1 <$> input




fullConnect input = ST.runST $ do
  counter <- newSTRef 0
  pointSets <- forM input $ \p -> (,) p <$> UF.newSet counter

  let dists = L.sortOn (\(d, _, _) -> d)
              [ (distance (p1, p2), (s1, s2), (p1, p2))
              | ((p1, s1), (p2, s2)) <- allPairs pointSets ]

  let go _ [] = error "No solution found"
      go remaining ((_, (s1, s2), (p1, p2)) : rest) = do
        eq <- UF.equal s1 s2
        if eq
          then go remaining rest
          else do
            UF.union s1 s2
            if remaining == 2
              then return (p1, p2)
              else go (remaining - 1) rest

  go (length input) dists


part2 :: Input -> Int
part2 input = x1 * x2
  where ((x1, _, _), (x2, _, _)) = fullConnect input
answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  print $ part2 inp
