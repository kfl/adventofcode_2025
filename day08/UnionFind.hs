{-# LANGUAGE LambdaCase #-}
module UnionFind
  ( Set
  , newSet
  , findRoot
  , size
  , equal
  , union
  ) where

import Control.Monad (unless, (>=>))
import Control.Monad.ST
import Data.STRef

data Cell s = Root !Int | Ptr (Set s)

type Set s = STRef s (STRef s (Cell s))

newSet :: ST s (Set s)
newSet = newSTRef (Root 1) >>= newSTRef

findRoot :: Set s -> ST s (Set s)
findRoot outer = readSTRef outer >>= readSTRef >>= \case
  Root _ -> pure outer
  Ptr next -> do
    root <- findRoot next
    readSTRef root >>= writeSTRef outer
    pure root

readRoot :: Set s -> ST s Int
readRoot = readSTRef >=> readSTRef >=> \case
  Root n -> pure n
  Ptr _  -> error "Impossible: expected Root"

size :: Set s -> ST s Int
size = findRoot >=> readRoot

equal :: Set s -> Set s -> ST s Bool
equal s1 s2 = (==) <$> (findRoot s1 >>= readSTRef) <*> (findRoot s2 >>= readSTRef)

union :: Set s -> Set s -> ST s ()
union s1 s2 = do
  (r1, n1) <- (,) <$> findRoot s1 <*> size s1
  (r2, n2) <- (,) <$> findRoot s2 <*> size s2
  eq <- equal r1 r2
  unless eq $ do
    let (parent, child) = if n1 > n2 then (r1, r2) else (r2, r1)
    readSTRef child >>= \i -> writeSTRef i (Ptr parent)
    readSTRef parent >>= \i -> writeSTRef i (Root (n1 + n2))
