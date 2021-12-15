-- ~\~ language=Haskell filename=app/Dijkstra.hs
-- ~\~ begin <<lit/day15.md|app/Dijkstra.hs>>[0]
{-# LANGUAGE TypeApplications #-}

module Dijkstra (minDist, minDistArray2) where

import RIO
import RIO.List.Partial (foldl1')
import qualified RIO.Set as Set
import RIO.Map ((!?))
import qualified RIO.Map as Map
import qualified Data.PQueue.Min as Q

import Data.Massiv.Array (Ix2(..))
import qualified Data.Massiv.Array as A
import qualified Data.Massiv.Array.Mutable as M

type Array2' r a = A.Array r Ix2 a
type Array2 a = Array2' A.U a

minOn :: Ord b => (a -> b) -> a -> a -> a
minOn f x y
    | f x < f y = x
    | otherwise = y

newtype DistLoc a = DistLoc (Ix2, a)
    deriving (Eq)

instance (Ord a) => Ord (DistLoc a) where
    compare (DistLoc (_, x)) (DistLoc (_, y)) = compare x y
toLoc (DistLoc (l, _)) = l

minDistArray2 :: forall a. (Ord a, Num a, Bounded a, A.Unbox a, M.Manifest A.U a)
              => Array2 a -> (Ix2 -> [Ix2]) -> Ix2 -> Ix2 -> a
minDistArray2 cost neighbours start end = runST go
  where go :: forall s. ST s a
        go = do
          unvisited <- M.newMArray @A.U @Bool size True
          dist      <- M.newMArray @A.U @a size maxBound
          M.write_ dist start 0
          let current :: Ix2 -> ST s a
              current n = fromMaybe maxBound <$> M.read dist n

              isUnvisited :: Ix2 -> ST s Bool
              isUnvisited i = fromMaybe False <$> M.read unvisited i

              updateDist :: Ix2 -> Ix2 -> ST s (DistLoc a)
              updateDist i j = do
                  v <- current j
                  x <- current i
                  return $ DistLoc (j, min v (x + fromMaybe maxBound (cost A.!? j)))

              recur :: Q.MinQueue (DistLoc a) -> Ix2 -> ST s a
              recur q pos
                | pos == end = fromMaybe maxBound <$> M.read dist end
                | otherwise  = do
                    unv <- isUnvisited pos
                    if unv then do
                        M.write_ unvisited pos False
                        unvisitedNeighbours <- filterM isUnvisited (neighbours pos)
                        newDists <- mapM (updateDist pos) unvisitedNeighbours
                        mapM_ (\(DistLoc (i, x)) -> M.write_ dist i x) newDists
                        let q' = foldl' (flip Q.insert) (Q.deleteMin q) newDists
                        recur q' (toLoc $ Q.findMin q')
                    else do
                        let q' = Q.deleteMin q
                        recur q' (toLoc $ Q.findMin q')
          recur Q.empty start
          where size = A.size cost

minDist :: forall n a. (Ord n, Ord a, Num a, Bounded a)
        => [n] -> (n -> [n]) -> (n -> n -> Maybe a) -> n -> n -> a
minDist nodes neighbours distance start end
  = go (Set.fromList nodes) (Set.singleton start) (Map.singleton start 0) start
  where go :: Set n -> Set n -> Map n a -> n -> a
        go unvisited marked dists pos
          | pos == end         = currentDist pos
          | Set.null unvisited = maxBound
          | otherwise          = go (pos `Set.delete` unvisited) newMarked newDists closestUnvisited
          where unvisitedNeighbours = filter (`Set.member` unvisited) (neighbours pos)
                currentDist node = fromMaybe maxBound (dists !? node)

                updateDist :: n -> Maybe a -> Maybe a
                updateDist node Nothing  = (+) <$> (dists !? pos) <*> distance pos node
                updateDist node x        = min <$> updateDist node Nothing <*> x

                newDists = foldr (\k -> Map.alter (updateDist k) k) dists unvisitedNeighbours
                newMarked = (pos `Set.delete` marked) <> Set.fromList unvisitedNeighbours
                closestUnvisited = foldl1' (minOn currentDist) $ Set.toList newMarked
-- ~\~ end
