-- ~\~ language=Haskell filename=app/Day09.hs
-- ~\~ begin <<lit/day09.md|app/Day09.hs>>[0]
module Day09 where

import RIO
import RIO.List (nub, sortBy)
import RIO.Char (ord)
import RIO.State (State, evalState, modify, get)
import RIO.ByteString (putStr)
import qualified RIO.Text as Text

import Parsing (digitArray, readInputParsing)

import Data.Massiv.Array (Array, Ix2(..))
import qualified Data.Massiv.Array as A
import qualified Data.Massiv.Array.Stencil as A.Stencil

import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet

import System.Random (mkStdGen, genWord8)

-- ~\~ begin <<lit/day09.md|parsing-day-9>>[0]
type Array2' r a = A.Array r A.Ix2 a
type Array2 a = Array2' A.U a

readInput :: (HasLogFunc env) => RIO env (Array2 Int)
readInput = readInputParsing "data/day09.txt" digitArray
-- ~\~ end
-- ~\~ begin <<lit/day09.md|solution-day-9>>[0]
neighbours :: [Ix2]
neighbours = [-1 :. 0, 1 :. 0, 0 :. -1, 0 :. 1]
-- ~\~ end
-- ~\~ begin <<lit/day09.md|solution-day-9>>[1]
findMinStencil :: A.Stencil Ix2 Int Int
findMinStencil = A.Stencil.makeStencil (A.Sz (3 :. 3)) (1 :. 1) go
    where go get
            | all ((value <) . get) neighbours = value + 1
            | otherwise                        = 0
            where value = get (0 :. 0)

solutionA :: Array2 Int -> Int
solutionA a = A.sum b
    where b :: Array2 Int
          b = A.compute $ A.Stencil.mapStencil (A.Fill 10) findMinStencil a
-- ~\~ end
-- ~\~ begin <<lit/day09.md|solution-day-9>>[2]
markBasins :: Array2 Int -> Array2 Int
markBasins a = evalState (A.mapM markNonZero a) 0
    where promise :: State Int (Array2 Int)
          promise = A.mapM markNonZero a
          markNonZero :: Int -> State Int Int
          markNonZero x
            | x /= 0    = modify (+ 1) >> get
            | otherwise = return 0
-- ~\~ end
-- ~\~ begin <<lit/day09.md|solution-day-9>>[3]
same :: (Eq a) => [a] -> Maybe a
same (a1:a2:as)
    | a1 == a2  = same (a2:as)
    | otherwise = Nothing
same [a]   = Just a
same _     = Nothing

watershedStencil :: A.Stencil Ix2 (Int, Int) (Int, Int)
watershedStencil = A.Stencil.makeStencil (A.Sz (3 :. 3)) (1 :. 1) go
    where go get
            | snd value /= 0 = value
            | otherwise      = paint color
            where value      = get (0 :. 0)
                  descending = filter (\p -> fst p < fst value) (get <$> neighbours)
                  color      = same $ snd <$> descending
                  paint (Just c) = (fst value, c)
                  paint _        = value
-- ~\~ end
-- ~\~ begin <<lit/day09.md|solution-day-9>>[4]
watershed :: Array2 (Int, Int) -> Array2 (Int, Int)
watershed = A.compute . A.Stencil.mapStencil (A.Fill (10, 0)) watershedStencil 

fixedPoint :: (Eq a) => (a -> a) -> a -> a
fixedPoint f x
    | x == next = x
    | otherwise = fixedPoint f next
    where next = f x

computeWatershed :: Array2 Int -> Array2 Int
computeWatershed a = A.compute $ A.map snd erase9
    where minima = A.compute $ A.Stencil.mapStencil (A.Fill 10) findMinStencil a 
          runWs  = fixedPoint watershed (A.compute $ A.zip a $ markBasins minima)
          erase9 = A.map (\(a, b) -> if a == 9 then (a, 0) else (a, b)) runWs
-- ~\~ end
-- ~\~ begin <<lit/day09.md|solution-day-9>>[5]
count :: Array2 Int -> MultiSet Int
count = A.foldMono MultiSet.singleton

solutionB :: Array2 Int -> Int
solutionB a = product $ take 3 $ sortBy (flip compare)
            $ map snd $ filter ((/= 0) . fst)
            $ MultiSet.toOccurList $ count
            $ computeWatershed a
-- ~\~ end
-- ~\~ begin <<lit/boilerplate.md|run-solutions>>[0]
runA :: (HasLogFunc env) => RIO env ()
runA = readInput >>= logInfo . display . tshow . solutionA 

runB :: (HasLogFunc env) => RIO env ()
runB = readInput >>= logInfo . display . tshow . solutionB
-- ~\~ end
-- ~\~ begin <<lit/day09.md|show-data-day-9>>[0]
printArray2 :: Array2 Int -> RIO env ()
printArray2 a =
    print $ Text.intercalate "\n" $ map (Text.intercalate " " . map tshow) (A.toLists2 a)
    where print = putStr . Text.encodeUtf8

showData :: IO ()
showData = runSimpleApp $ do
    inp <- readInput
    printArray2 inp
    print "\n\n"
    printArray2 $ A.compute $ A.map randomize $ computeWatershed inp
    where print = putStr . Text.encodeUtf8
          randomize n
            | n == 0    = 0
            | otherwise = fromIntegral . fst . genWord8 . mkStdGen $ n
-- ~\~ end
-- ~\~ end
