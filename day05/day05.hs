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


{-

Idea 1: Just use the right data structure and everything will
work. The right data structure in this case is interval sets.

-}

range (x,y) = fromIntegral x <=..<= fromIntegral y

part2 :: Input -> Int
part2 (ranges, _) = sum $ map ((+1) . IS.width) $ IS.toList $ IS.fromList $ map range ranges

answer2 = part2 <$> input


{-

Idea 2: Don't rely on external libraries, instead work directly with
ranges. The algorithm turns out to be quite manageable: Sort the
ranges and merge overlapping/adjacent ranges.

Curiously, it turns out to important to use foldl' and not foldr in
`mergeRanges` for correctness and not performance. Because the input
is sorted by start, when you see a new range `r@(lo2, up2)`, the only
candidate it can overlap/adjacent with is the most recent merged range
(the "rightmost so far"). And crucially: When you merge in a left
fold, you only extend the end to the right. You do not move the start
left.

-}

mergeRanges :: [Range] -> [Range]
mergeRanges ranges = L.foldl' step [] $ L.sort ranges
  where
    step [] r = [r]
    step acc@((lo1, up1) : rs) r@(lo2, up2)
      | up1 + 1 >= lo2 = (lo1, max up1 up2) : rs -- overlap/adjacent
      | otherwise      = r : acc

part2' :: Input -> Int
part2' (ranges, _) = sum $ map (\(lo, up) -> up - lo + 1) $ mergeRanges ranges

answer2' = part2' <$> input


main = do
  inp <- input
  print $ part1 inp
  print $ part2 inp
