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

import Control.Monad (forM_)
import qualified Data.SBV as S
import Data.SBV ((.==), (.>=))


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


solveMachine :: Machine -> IO Integer
solveMachine (_, buttons, joltage) = do
  S.LexicographicResult res <- S.optimize S.Lexicographic $ do

    -- variables for the number of times each button is pressed
    xs <- S.sIntegers ["button_" ++ show i | i <- [0..length buttons - 1]]

    -- We cannot press a button a negative amount of times
    S.constrain $ S.sAll (.>= 0) xs

    -- Pressing the buttons must generate the correct amount of joltage
    forM_ (zip [0..] joltage) $ \(i, j) ->
      S.constrain $ sum [x | (x, btn) <- zip xs buttons, i `elem` btn]
                    .== fromIntegral j

    S.minimize "presses" (sum xs)

  return $ fromJust $ S.getModelValue "presses" res

part2 :: Input -> IO Integer
part2 input = sum <$> mapM solveMachine input
answer2 = part2 =<< input

main = do
  inp <- input
  print $ part1 inp
  print =<< part2 inp
