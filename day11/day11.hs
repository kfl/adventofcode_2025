{-# LANGUAGE LambdaCase #-}
module Main where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map as Map
import Data.Map (Map)

test =  map parse [ "aaa: you hhh"
                  , "you: bbb ccc"
                  , "bbb: ddd eee"
                  , "ccc: ddd eee fff"
                  , "ddd: ggg"
                  , "eee: out"
                  , "fff: out"
                  , "ggg: out"
                  , "hhh: ccc fff iii"
                  , "iii: out"
                  ]
input = map parse . lines <$> readFile "input.txt"

type Device = String
type Network = Map Device [Device]
type Input = [(Device, [Device])]

parse :: String -> (Device, [Device])
parse str = (dev, words outs)
  where (dev, _ : outs) = L.break (== ':') str

-- Another example
--     A → B → D
--     ↓   ↓   ↑
--     C → E → F
--         ↓
--         D
exampl = Map.fromList [ ("A", ["B", "C"])
                      , ("B", ["D", "E"])
                      , ("C", ["E"])
                      , ("E", ["D", "F"])
                      , ("D", ["F"])
                      , ("F", [])
                      ]

countPaths :: Network -> Device -> Device -> Int
countPaths network source target = paths Map.! source
  where
    -- All devices in the network, duplicates are OK
    allDevices = source : target : Map.keys network ++ concat (Map.elems network)

    -- Lazy memo table - counts computed on demand
    paths = Map.fromList [(device, dfs device) | device <- allDevices]

    -- Compute number paths for a given device to the target device using the memo table recursively
    dfs device
      | device == target = 1
      | otherwise = sum [paths Map.! out | out <- Map.findWithDefault [] device network]

part1 :: Input -> Int
part1 input = countPaths (Map.fromList input) "you" "out"
answer1 = part1 <$> input

part2 :: Input -> Int
part2 input = first * middle * last
  where
    count = countPaths $ Map.fromList input
    (first, middle, last) = case count "fft" "dac" of
                              0 -> (count "svr" "dac", count "dac" "fft", count "fft" "out")
                              m -> (count "svr" "fft", m, count "dac" "out")
answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  print $ part2 inp
