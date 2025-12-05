{-# LANGUAGE LambdaCase #-}
module Main where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.List.Split as L
import Data.Bifunctor (bimap)
import Data.Ix (inRange)

import qualified Data.IntervalSet as IS
import qualified Data.Interval as IS
import Data.Interval ((<=..<=))

test =  parse $ unlines [ "3-5"
                        , "10-14"
                        , "16-20"
                        , "12-18"
                        , ""
                        , "1"
                        , "5"
                        , "8"
                        , "11"
                        , "17"
                        , "32"
                        ]
input = parse <$> readFile "input.txt"
type Range = (Int, Int)
type ID = Int
type Input = ([Range], [ID])

parse :: String -> Input
parse str = (map parseRange ranges, map read ids)
  where [ranges, ids] = map lines $ L.splitOn "\n\n" str
        parseRange s = bimap read (read . drop 1) $ L.break (== '-') s

part1 :: Input -> Int
part1 (ranges, ids) = length $ filter (\id -> any (`inRange` id) ranges) ids

answer1 = part1 <$> input

range (x,y) = fromIntegral x <=..<= fromIntegral y

part2 :: Input -> Int
part2 (ranges, _) = sum $ map ((+1) . IS.width) $ IS.toList $ IS.fromList $ map range ranges

answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  print $ part2 inp
