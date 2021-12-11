# Day 11: Dumbo Octopus

``` {.haskell file=app/Day11.hs}
module Day11 where

import RIO
import RIO.State (evalState, MonadState, modify, gets)
import Data.Massiv.Array (Ix2(..))
import qualified Data.Massiv.Array as A
import Parsing (Parser, sepEndBy1, failOnException, eol, digit, readInputParsing)

<<parser-day-11>>
<<solution-day-11>>
<<run-solutions>>
```

``` {.haskell #parser-day-11}
<<digit-array-parser>>

readInput :: (HasLogFunc env) => RIO env (Array2 Int)
readInput = readInputParsing "data/day11.txt" digitArray
```

``` {.haskell #solution-day-11}
step :: (MonadState (Array2 Int) m) => m Int
step = clock >> flash >> reset
```

``` {.haskell #solution-day-11}
clock :: (MonadState (Array2 Int) m) => m ()
clock = modify (A.compute . A.map (+ 1))
```

``` {.haskell #solution-day-11}
home :: A.Ix2
home = 0 :. 0

neighbours :: [A.Ix2]
neighbours = [ -1 :. -1, 0 :. -1, 1 :. -1
             , -1 :.  0,          1 :.  0
             , -1 :.  1, 0 :.  1, 1 :.  1 ]

count :: (a -> Bool) -> [a] -> Int
count f = sum . map (\x -> if f x then 1 else 0)

flashed :: Int -> Bool
flashed c = c > 9 && c <= 1000

flashStencil :: A.Stencil Ix2 Int Int
flashStencil = A.makeStencil (A.Sz (3 :. 3)) (1 :. 1) go
    where go get = if flashed (get home) then 1000
                   else get home + count (flashed . get) neighbours

countArray :: (A.Unbox a) => (a -> Bool) -> Array2 a -> Int
countArray f = A.sum . A.map (\x -> if f x then 1 else 0)

flash :: MonadState (Array2 Int) m => m ()
flash = do
    n <- gets $ countArray flashed
    if n == 0 then return ()
    else modify go >> flash
    where go :: Array2 Int -> Array2 Int
          go = A.compute . A.mapStencil (A.Fill 0) flashStencil

reset :: MonadState (Array2 Int) m => m Int
reset = do
    n <- gets $ countArray (>= 1000)
    modify $ A.compute . A.map (\x -> if x >= 1000 then 0 else x)
    return n

repeatM :: (Applicative m) => Int -> m a -> m [a]
repeatM n a = loop n
    where loop n
            | n <= 0    = pure []
            | otherwise = liftA2 (:) a (loop (n - 1))
```

``` {.haskell #solution-day-11}
solutionA :: Array2 Int -> Int
solutionA = sum . evalState (repeatM 100 step)

solutionB :: Array2 Int -> Int
solutionB = const 0
```
