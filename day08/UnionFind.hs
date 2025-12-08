module UnionFind
  ( Set
  , newSet
  , findRoot
  , size
  , equal
  , union
  ) where

import Control.Monad (unless)
import Control.Monad.ST
import GHC.STRef

data Cell s = Root !Int !Int   -- id, size
            | Ptr (Set s)

data Set s = Set !Int (STRef s (STRef s (Cell s)))

instance Eq (Set s) where
  Set i _ == Set j _ = i == j

newSet :: STRef s Int -> ST s (Set s)
newSet counter = do
  i <- readSTRef counter
  writeSTRef counter (i + 1)
  inner <- newSTRef (Root i 1)
  outer <- newSTRef inner
  return (Set i outer)

findRoot :: Set s -> ST s (Set s)
findRoot (Set _ outerRef) = do
  innerRef <- readSTRef outerRef
  cell <- readSTRef innerRef
  case cell of
    Root i _ -> return (Set i outerRef)  -- use id from Root cell
    Ptr next -> do
      rootSet@(Set _ rootOuterRef) <- findRoot next
      rootInnerRef <- readSTRef rootOuterRef
      writeSTRef outerRef rootInnerRef
      return rootSet

readRoot :: Set s -> ST s (Int, Int)
readRoot (Set _ outerRef) = do
  innerRef <- readSTRef outerRef
  cell <- readSTRef innerRef
  case cell of
    Root i n -> return (i, n)
    Ptr _    -> error "Impossible: expected Root"

size :: Set s -> ST s Int
size s = snd <$> (findRoot s >>= readRoot)

equal :: Set s -> Set s -> ST s Bool
equal i j = do
  Set idI _ <- findRoot i
  Set idJ _ <- findRoot j
  return (idI == idJ)

setRoot :: Int -> Set s -> Set s -> ST s ()
setRoot n r1@(Set id1 outer1) (Set _ outer2) = do
  inner1 <- readSTRef outer1
  inner2 <- readSTRef outer2
  writeSTRef inner2 (Ptr r1)
  writeSTRef inner1 (Root id1 n)

union :: Set s -> Set s -> ST s ()
union s1 s2 = do
  r1 <- findRoot s1
  r2 <- findRoot s2
  (id1, n1) <- readRoot r1
  (id2, n2) <- readRoot r2
  unless (id1 == id2) $ do
    let n = n1 + n2
    if n1 > n2
      then setRoot n r1 r2
      else setRoot n r2 r1
