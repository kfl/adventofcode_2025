{-# LANGUAGE LambdaCase, MultilineStrings #-}
module Main where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.List.Split as L
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

test = parse [ "123 328  51 64 "
             , " 45 64  387 23 "
             , "  6 98  215 314"
             , "*   +   *   +  "
             ]
input = parse . lines <$> readFile "input.txt"

type Problem = [String]
type Input = [Problem]

parse :: [String] -> Input
parse strs = map L.transpose columns
  where columns = L.splitWhen (all (' ' ==)) (L.transpose strs)

str2opr :: String -> [Integer] -> Integer
str2opr = \case '+':_ -> sum; _ -> product

part1 :: Input -> Integer
part1 problems = sum $ map solve problems
  where
    solve problem = str2opr opr $ map read xs
      where Just (xs, opr) = L.unsnoc problem
answer1 = part1 <$> input

part2 :: Input -> Integer
part2 problems = sum $ map solve problems
  where
    solve problem = str2opr opr $ right2left xs
      where Just (xs, opr) = L.unsnoc problem
            right2left = map read . reverse . L.transpose

answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  print $ part2 inp
