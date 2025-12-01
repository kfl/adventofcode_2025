{-# LANGUAGE LambdaCase #-}
module Main where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

test =  map parse [ "L68"
                  , "L30"
                  , "R48"
                  , "L5"
                  , "R60"
                  , "L55"
                  , "L1"
                  , "L99"
                  , "R14"
                  , "L82"
                  ]
input = map parse . lines <$> readFile "input.txt"

type Input = [Int]

parse :: String -> Int
parse (r : n) = (case r of 'L' -> negate ; _ -> id) $ read n

count p = length . filter p

part1 :: Input -> Int
part1 input = count (==0) $ L.scanl' doit 50 input
  where doit d n = (d + n) `mod` 100
answer1 = part1 <$> input

part2 :: Input -> Int
part2 input = snd $ L.foldl' doit (50, 0) input
  where doit (d, cz) n = (r, cz + abs q + corr)
          where
            s = d + n
            (q, r) = s `divMod` 100
            corr = if s <= 0 && r == 0 then 1
                   else if s < 0 && d == 0 then -1
                        else 0
answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  print $ part2 inp
