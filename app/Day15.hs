-- ~\~ language=Haskell filename=app/Day15.hs
-- ~\~ begin <<lit/day15.md|app/Day15.hs>>[0]
module Day15 where

import RIO
import Data.Massiv.Array (Ix2(..))
import qualified Data.Massiv.Array as A
import Parsing (readInputParsing, digitArray)
import Dijkstra (minDist)

-- ~\~ begin <<lit/day15.md|parser-day15>>[0]
type Array2' r a = A.Array r A.Ix2 a
type Array2 a = Array2' A.U a

readInput :: (HasLogFunc env) => RIO env (Array2 Int)
readInput = readInputParsing "data/day15.txt" digitArray
-- ~\~ end

listNodes :: (A.Unbox a) => Array2 a -> [Ix2]
listNodes = A.toList . A.imap const

neighbours :: (A.Unbox a) => Array2 a -> Ix2 -> [Ix2]
neighbours x i = filter (isJust . (x A.!?)) $ map (+ i) n
    where n = [-1 :. 0, 1 :. 0, 0 :. -1, 0 :. 1]

distance :: Array2 Int -> Ix2 -> Ix2 -> Maybe Int
distance x _ i = x A.!? i

endPoint :: Array2 Int -> Ix2
endPoint x = A.unSz (A.size x) - (1 :. 1)

solutionA :: Array2 Int -> Int
solutionA inp = minDist (listNodes inp) (neighbours inp) (distance inp) (0 :. 0) (endPoint inp)

scaleUp :: Array2 Int -> Array2 Int
scaleUp x = stack rowM
    where rowM = foldl' (\r t -> A.compute $ A.append' 1 r t) x (map (\h -> A.map (inc h) x) [1..4])
          stack row = foldl' (\r t -> A.compute $ A.append' 2 r t) row (map (\h -> A.map (inc h) row) [1..4])
          inc h x = (x - 1 + h) `mod` 9 + 1

solutionB :: Array2 Int -> Int
solutionB inp' = minDist (listNodes inp) (neighbours inp) (distance inp) (0 :. 0) (endPoint inp)
    where inp = scaleUp inp'

-- ~\~ begin <<lit/boilerplate.md|run-solutions>>[0]
runA :: (HasLogFunc env) => RIO env ()
runA = readInput >>= logInfo . display . tshow . solutionA 

runB :: (HasLogFunc env) => RIO env ()
runB = readInput >>= logInfo . display . tshow . solutionB
-- ~\~ end
-- ~\~ end
