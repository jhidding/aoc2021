# Day 9: Smoke Basin
Lava tubes and more hydrothermal vents! I'll be doing this in `Massiv` again. Here is a rendering of my input data.

``` {.gnuplot output=fig/day09-input.svg}
The input data.
---
set xrange [0:100]
set yrange [0:100]
set size square
plot 'data/day09-output.txt' i 0 matrix w image
```

``` {.haskell file=app/Day09.hs}
module Day09 where

import RIO
import RIO.List (nub, sortBy)
import RIO.Char (ord)
import RIO.State (State, evalState, modify, get)
import RIO.ByteString (putStr)
import qualified RIO.Text as Text

import Parsing (Parser, sepEndBy1, eol, failOnException, readInputParsing)
import Text.Megaparsec.Char (digitChar) 

import Data.Massiv.Array (Array, Ix2(..))
import qualified Data.Massiv.Array as A
import qualified Data.Massiv.Array.Stencil as A.Stencil

import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet

import System.Random (mkStdGen, genWord8)

<<parsing-day-9>>
<<solution-day-9>>
<<run-solutions>>
<<show-data-day-9>>
```

Today's input data is given as digits between 0 and 9.

``` {.haskell #parsing-day-9}
type Array2' r a = Array r Ix2 a
type Array2 a = Array2' A.U a

digit :: Parser Int
digit = toValue <$> digitChar
    where toValue c = ord c - ord '0'

heightMapP :: Parser (Array2 Int)
heightMapP = sepEndBy1 (some digit) eol >>= toArray2
    where toArray2 = failOnException . A.fromListsM A.Seq

readInput :: (HasLogFunc env) => RIO env (Array2 Int)
readInput = readInputParsing "data/day09.txt" heightMapP
```

I'll be using `Massiv`s stencil interface to solve this. Each stencil works on a neighbourhood of four pixels directly north, south, west and east from current location:

``` {.haskell #solution-day-9}
neighbours :: [Ix2]
neighbours = [-1 :. 0, 1 :. 0, 0 :. -1, 0 :. 1]
```

For part A, we need to find the minima in the data.

``` {.haskell #solution-day-9}
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
```

In part B, we need to compute the watershed of the height map.

1. Mark minima.
2. Grow to a neighbourhood around each minimum:
    - stop when two patches meet
    - otherwise, repeat

We start by marking all minima found in part A with a unique integer identifier. I use a monadic map to give each minimum a number > 0.

``` {.haskell #solution-day-9}
markBasins :: Array2 Int -> Array2 Int
markBasins a = evalState (A.mapM markNonZero a) 0
    where promise :: State Int (Array2 Int)
          promise = A.mapM markNonZero a
          markNonZero :: Int -> State Int Int
          markNonZero x
            | x /= 0    = modify (+ 1) >> get
            | otherwise = return 0
```

The second step, we paint a pixel if all descending pixels have the same color. If a pixel is already colored, we leave it alone.

``` {.haskell #solution-day-9}
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
```

We keep doing this, until the watershed doesn't change anymore. Afterwards, we need to clear pixels where the value is 9, this only happens at the edges.

``` {.haskell #solution-day-9}
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
```

To get our answer, we need to measure the size of each patch, and then find the three largest ones. On Day 6 we already saw the `MultiSet` in use, now again so:

``` {.haskell #solution-day-9}
count :: Array2 Int -> MultiSet Int
count = A.foldMono MultiSet.singleton

solutionB :: Array2 Int -> Int
solutionB a = product $ take 3 $ sortBy (flip compare)
            $ map snd $ filter ((/= 0) . fst)
            $ MultiSet.toOccurList $ count
            $ computeWatershed a
```

``` {.haskell #show-data-day-9 .hide}
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
```

Here is my rendering of the resulting watershed:

``` {.gnuplot output=fig/day09-output.svg}
The watershed segmentation of the input data.
---
set xrange [0:100]
set yrange [0:100]
set size square
plot 'data/day09-output.txt' i 1 matrix w image
```

