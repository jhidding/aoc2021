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

-- ~\~ begin <<lit/day15.md|dijkstra-imports>>[0]
import Data.Massiv.Array (Ix2(..))
import qualified Data.Massiv.Array as A
-- ~\~ end
-- ~\~ begin <<lit/day15.md|dijkstra-generic>>[0]
newtype DistLoc i a = DistLoc (i, a)
    deriving (Eq)

instance (Ord a, Eq i) => Ord (DistLoc i a) where
    compare (DistLoc (_, x)) (DistLoc (_, y)) = compare x y

toLoc :: DistLoc i a -> i
toLoc (DistLoc (l, _)) = l
-- ~\~ end
-- ~\~ begin <<lit/day15.md|dijkstra-generic>>[1]
minDist :: forall n m a. (Ord n, Ord a, Num a, Bounded a)
        => (n -> [(m, a)]) -> (n -> m -> n) -> n -> n -> Maybe a
minDist neighbours move start end
  = go Set.empty (Q.singleton $ DistLoc (start, 0)) (Map.singleton start 0) start
  where go :: Set n -> Q.MinQueue (DistLoc n a) -> Map n a -> n -> Maybe a
        go visited marked dists pos
          | pos == end         = Just $ currentDist pos
          | pos `Set.member` visited = go visited marked'' dists . toLoc =<< Q.getMin marked''
          | otherwise          = go (pos `Set.insert` visited) marked' dists' . toLoc =<< Q.getMin marked'
          where unvisitedEdges = filter ( (`Set.notMember` visited) 
                                        . move pos . fst ) (neighbours pos)
                currentDist node = fromMaybe maxBound (dists !? node)

                distLoc :: (m, a) -> DistLoc n a
                distLoc (edge, cost) = DistLoc (node, min v (w + cost))
                    where node = move pos edge
                          v = currentDist node
                          w = currentDist pos

                newDists = map distLoc unvisitedEdges
                dists' = foldl' (\m (DistLoc (i, x)) -> Map.insert i x m) dists newDists

                marked' :: Q.MinQueue (DistLoc n a)
                marked' = Q.deleteMin marked <> Q.fromList newDists
                marked'' = Q.deleteMin marked
-- ~\~ end
-- ~\~ begin <<lit/day15.md|dijkstra-array>>[0]
type Array2' r a = A.Array r Ix2 a
type Array2 a = Array2' A.U a

-- ~\~ begin <<lit/day15.md|condM>>[0]
condM :: (Monad m) => [(m Bool, m a)] -> m a
condM ((pred, action): cs) = do
    c <- pred
    if c then action else condM cs
condM [] = error "no matching conditions"

otherwiseM :: (Monad m) => m Bool
otherwiseM = pure True
-- ~\~ end

minDistArray2 :: forall a. (Ord a, Num a, Bounded a, A.Unbox a, A.Manifest A.U a)
              => Array2 a -> (Ix2 -> [Ix2]) -> Ix2 -> Ix2 -> a
minDistArray2 cost neighbours start end = runST go
  where go :: forall s. ST s a
        go = do
          -- ~\~ begin <<lit/day15.md|dijkstra-array-init>>[0]
          unvisited <- A.newMArray @A.U @Bool size True
          dist      <- A.newMArray @A.U @a size maxBound
          A.write_ dist start 0
          -- ~\~ end

          let
              -- ~\~ begin <<lit/day15.md|dijkstra-array-distloc>>[0]
              distLoc :: Ix2 -> Ix2 -> ST s (DistLoc Ix2 a)
              distLoc i j = do
                  v <- A.readM dist j
                  x <- A.readM dist i
                  return $ DistLoc (j, min v (x + (cost A.! j)))
              -- ~\~ end
              -- ~\~ begin <<lit/day15.md|dijkstra-array-recur>>[0]
              recur :: Q.MinQueue (DistLoc Ix2 a) -> Ix2 -> ST s a
              recur q pos = condM
                [ (pure $ pos == end, A.readM dist end)
                , (A.readM unvisited pos, do
                     A.write_ unvisited pos False
                     unvisitedNeighbours <- filterM (A.readM unvisited) (neighbours pos)
                     newDists <- mapM (distLoc pos) unvisitedNeighbours
                     mapM_ (\(DistLoc (i, x)) -> A.write_ dist i x) newDists
                     let q' = foldl' (flip Q.insert) (Q.deleteMin q) newDists
                     recur q' (toLoc $ Q.findMin q'))
                , (otherwiseM, do
                     let q' = Q.deleteMin q
                     recur q' (toLoc $ Q.findMin q')) ]
              -- ~\~ end

          recur Q.empty start
          where size = A.size cost
-- ~\~ end
-- ~\~ end
