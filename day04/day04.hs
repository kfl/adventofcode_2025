{-# LANGUAGE MultilineStrings #-}
module Main where

import qualified Data.List as L
import qualified Data.Array.Unboxed as U
import Data.Array.Unboxed ((!), (!?))
import Data.Maybe (mapMaybe)

test = parse $ lines
  """
  ..@@.@@@@.
  @@@.@.@.@@
  @@@@@.@.@@
  @.@@@@..@.
  @@.@@@@.@@
  .@@@@@@@.@
  .@.@.@.@@@
  @.@@@.@@@@
  .@@@@@@@@.
  @.@.@@@.@.
  """
input = parse . lines <$> readFile "input.txt"

type Row = Int
type Col = Int
type Pos = (Col, Row)
type Grid = U.Array Pos Char
type Input = Grid

parse :: [String] -> Grid
parse rows@(cols : _) = U.array bounds letters
  where letters = [ ((x,y), c) | (y, row) <- zip [0..] rows
                               , (x, c) <- zip [0..] row]
        bounds = ((0,0), (length cols - 1, length rows - 1))


adjacents = [ idx | idx <- U.range ((-1, -1), (1,1)), idx /= (0,0) ]
(a, b) |+| (c, d) = (a+c, b+d)
neighbours pos = [ pos |+| adj | adj <- adjacents ]

count p = length . filter p

accessable grid = [ pos | (pos, '@') <- U.assocs grid
                        , 4 > count (=='@') (mapMaybe (grid !?) (neighbours pos)) ]

part1 :: Input -> Int
part1 = length . accessable
answer1 = part1 <$> input

part2 :: Input -> Int
part2 = sum . L.unfoldr (\grid -> case accessable grid of
                                    [] -> Nothing
                                    acc -> Just (length acc, grid U.// [(p, '.') | p <- acc]))
answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  print $ part2 inp
