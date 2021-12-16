-- ~\~ language=Haskell filename=app/Day15.hs
-- ~\~ begin <<lit/day15.md|app/Day15.hs>>[0]
module Day15 where

import RIO
import Data.Massiv.Array (Ix2(..))
import qualified Data.Massiv.Array as A
import Parsing (readInputParsing, digitArray)
import Dijkstra (minDistArray2, minDist)

-- ~\~ begin <<lit/day15.md|parser-day15>>[0]
type Array2' r a = A.Array r A.Ix2 a
type Array2 a = Array2' A.U a

readInput :: (HasLogFunc env) => RIO env (Array2 Int)
readInput = readInputParsing "data/day15.txt" digitArray
-- ~\~ end
-- ~\~ begin <<lit/day15.md|solution-day15>>[0]
neighbours :: (A.Unbox a) => Array2 a -> Ix2 -> [Ix2]
neighbours x i = filter (isJust . (x A.!?)) $ map (+ i) n
    where n = [-1 :. 0, 1 :. 0, 0 :. -1, 0 :. 1]

distance :: Array2 Int -> Ix2 -> Ix2 -> Maybe Int
distance x _ i = x A.!? i

endPoint :: Array2 Int -> Ix2
endPoint x = A.unSz (A.size x) - (1 :. 1)

solutionA :: Array2 Int -> Int
solutionA inp = minDistArray2 inp (neighbours inp) (0 :. 0) (endPoint inp)

scaleUp :: Array2 Int -> Array2 Int
scaleUp x = stack 2 (stack 1 x)
    where stack axis row = foldl' (\r t -> A.compute $ A.append' axis r t) row
                         $ map (\h -> A.map (inc h) row) [1..4]
          inc h x = (x - 1 + h) `mod` 9 + 1

solutionB :: Array2 Int -> Int
-- solutionB inp' = minDist (neighbours inp) (distance inp) (0 :. 0) (endPoint inp)
solutionB inp' = minDistArray2 inp (neighbours inp) (0 :. 0) (endPoint inp)
    where inp = scaleUp inp'
-- ~\~ end
-- ~\~ begin <<lit/boilerplate.md|run-solutions>>[0]
runA :: (HasLogFunc env) => RIO env ()
runA = readInput >>= logInfo . display . tshow . solutionA 

runB :: (HasLogFunc env) => RIO env ()
runB = readInput >>= logInfo . display . tshow . solutionB
-- ~\~ end
-- ~\~ end
