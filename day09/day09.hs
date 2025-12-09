{-# LANGUAGE LambdaCase #-}
module Main where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

test =  map parse [ "7,1"
                  , "11,1"
                  , "11,7"
                  , "9,7"
                  , "9,5"
                  , "2,5"
                  , "2,3"
                  , "7,3"
                  ]
input = map parse . lines <$> readFile "input.txt"

type Coor = (Int, Int)
type Input = [Coor]

parse :: String -> Coor
parse str = read $ "("<> str <>")"

area (a,b) (c,d) = (abs(a - c) + 1) * (abs(b - d) + 1)

allPairs :: [a] -> [(a,a)]
allPairs xs = [ (x, y) | (x:ys) <- L.tails xs, y <- ys ]

part1 :: Input -> Int
part1 input = maximum [ area c1 c2 | (c1, c2) <- allPairs input ]
answer1 = part1 <$> input



type Edge = (Coor, Coor)
type Rect = (Coor, Coor)
type Polygon = [Coor]

close cs = zip cs (drop 1 cs ++ take 1 cs)

polygonEdges :: Polygon -> [Edge]
polygonEdges = close

cross :: Coor -> Coor -> Coor -> Int
cross (ax, ay) (bx, by) (cx, cy) =
    (bx - ax) * (cy - ay) - (by - ay) * (cx - ax)

-- Two edges AB and CD intersect if C and D are on opposite sides of line AB,
-- and A and B are on opposite sides of line CD. Cross product gives the side.
-- Returns False for touching/overlapping edges (boundary handled separately).
edgesIntersect :: Edge -> Edge -> Bool
edgesIntersect (a, b) (c, d) =
    oppositeSides a b c d && oppositeSides c d a b
  where
    oppositeSides p q r s =
        let cr = cross p q r
            cs = cross p q s
        in (cr > 0 && cs < 0) || (cr < 0 && cs > 0)

pointInPolygon :: Coor -> [Edge] -> Bool
pointInPolygon (px, py) = go 0
  where
    go :: Int -> [Edge] -> Bool
    go !count [] = odd count
    go !count (((x1, y1), (x2, y2)) : rest)
        | onEdge    = True
        | crosses   = go (count + 1) rest
        | otherwise = go count rest
      where
        onEdge =
            cross (x1, y1) (x2, y2) (px, py) == 0 &&
            px >= min x1 x2 && px <= max x1 x2 &&
            py >= min y1 y2 && py <= max y1 y2

        crosses
            | y1 == y2        = False
            | py < min y1 y2  = False
            | py >= max y1 y2 = False
            | otherwise       = px < x1 + (py - y1) * (x2 - x1) `div` (y2 - y1)

rectCorners :: Rect -> [Coor]
rectCorners ((x1, y1), (x2, y2)) =
    [(x1, y1), (x2, y1), (x2, y2), (x1, y2)]

rectEdges :: Rect -> [Edge]
rectEdges r = close $ rectCorners r

rectInsidePolygon :: Rect -> [Edge] -> Bool
rectInsidePolygon rect polyEdges =
    all (`pointInPolygon` polyEdges) (rectCorners rect) &&
    not (any edgeCrossesPoly (rectEdges rect))
  where
    edgeCrossesPoly re = any (edgesIntersect re) polyEdges

part2 :: Input -> Int
part2 input = maximum [ area c1 c2 | rect@(c1, c2) <- allPairs input
                                   , rectInsidePolygon rect polyEdges ]
  where polyEdges = polygonEdges input

answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  print $ part2 inp
