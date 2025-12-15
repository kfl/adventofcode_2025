{-# LANGUAGE LambdaCase, MultilineStrings #-}
module Main where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.List.Split as L
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Bifunctor (bimap)


test = parse
        """
        0:
        ###
        ##.
        ##.

        1:
        ###
        ##.
        .##

        2:
        .##
        ###
        ##.

        3:
        ##.
        ###
        ##.

        4:
        ###
        #..
        ###

        5:
        ###
        .#.
        ###

        4x4: 0 0 0 0 2 0
        12x5: 1 0 1 0 2 2
        12x5: 1 0 1 0 3 2
        """
input = parse <$> readFile "input.txt"

type Shape = [[Char]]
type Region = ((Int, Int), [Int])
type Input = ([Shape], [Region])

parse :: String -> Input
parse str = (map parseShape shapes, map parseRegion $ lines regions)
  where (shapes, [regions]) = L.span (\case _:':':_ -> True; _ -> False) $ L.splitOn "\n\n" str
        parseShape = drop 1 . lines
        parseRegion = bimap parseSpec (map read . words . drop 1) . L.break (== ':')
        parseSpec = bimap read (read . drop 1) . L.break (== 'x')

count p = length . filter p

shapeSize shape = sum $ map (count (== '#')) shape

part1 :: Input -> Int
part1 (shapes, regions) = count notImpossible regions
  where sizes = Map.fromList [(i, shapeSize s) | (i, s) <- zip [0..] shapes]
        -- Estimate: if the area is bigger than the requested quantities then it is not impossible
        notImpossible ((w, h), quants) = w*h >= sum [ q * sizes Map.! i | (i, q) <- zip [0..] quants]
answer1 = part1 <$> input

-- part2 :: Input -> Int
-- part2 input = undefined
-- answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  -- print $ part2 inp
