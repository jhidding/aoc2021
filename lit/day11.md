# Day 11: Dumbo Octopus
This is clearly inspired on [this demo of spontaneously synchronising fireflies](https://ncase.me/fireflies/).

``` {.haskell file=app/Day11.hs}
module Day11 where

import RIO
import RIO.ByteString (putStr)
import qualified RIO.Text as Text
import RIO.State (evalStateT, evalState, execState, MonadState, modify, get, gets)
import Data.Massiv.Array (Ix2(..))
import qualified Data.Massiv.Array as A
import Parsing (Parser, sepEndBy1, failOnException, eol, digit, readInputParsing)

<<parser-day-11>>
<<solution-day-11>>
<<show-data-day-11>>
<<run-solutions>>
```

We can reuse the input parser from day 9.

``` {.haskell #parser-day-11}
<<digit-array-parser>>

readInput :: (HasLogFunc env) => RIO env (Array2 Int)
readInput = readInputParsing "data/day11.txt" digitArray
```

Each iteration can be divided in three steps:

* `clock`: advance the cycle of every octopus by one
* `flash`: resolve the flashing, marking flashed octopusses
* `reset`: reset the counter for flashed octopusses

I put these steps into a state monad.

``` {.haskell #solution-day-11}
step :: (MonadState (Array2 Int) m) => m Int
step = clock >> flash >> reset
```

The `clock` advances every counter by one tick.

``` {.haskell #solution-day-11}
clock :: (MonadState (Array2 Int) m) => m ()
clock = modify (A.compute . A.map (+ 1))
```

To resolve the flashes, I use my friend the *stencil* again. I mark flashed octopusses by setting their counter to 1000. That way, they don't get counted twice.

``` {.haskell #solution-day-11}
home :: A.Ix2
home = 0 :. 0

neighbours :: [A.Ix2]
neighbours = [ -1 :. -1, 0 :. -1, 1 :. -1
             , -1 :.  0,          1 :.  0
             , -1 :.  1, 0 :.  1, 1 :.  1 ]

count :: (a -> Bool) -> [a] -> Int
count f = sum . map (\x -> if f x then 1 else 0)

countArray :: (A.Unbox a) => (a -> Bool) -> Array2 a -> Int
countArray f = A.sum . A.map (\x -> if f x then 1 else 0)

flashed :: Int -> Bool
flashed c = c > 9 && c < 1000

flashStencil :: A.Stencil Ix2 Int Int
flashStencil = A.makeStencil (A.Sz (3 :. 3)) (1 :. 1) go
    where go get = if flashed (get home) then 1000
                   else get home + count (flashed . get) neighbours

flash :: MonadState (Array2 Int) m => m ()
flash = do
    n <- gets $ countArray flashed
    if n == 0 then return ()
    else modify go >> flash
    where go :: Array2 Int -> Array2 Int
          go = A.compute . A.mapStencil (A.Fill 0) flashStencil
```

At the `reset`, I count how many values are larger than 1000, and set them back to 0.

``` {.haskell #solution-day-11}
reset :: MonadState (Array2 Int) m => m Int
reset = do
    n <- gets $ countArray (>= 1000)
    modify $ A.compute . A.map (\x -> if x >= 1000 then 0 else x)
    return n
```

I put everything in a state monad. The parts A and B have different stopping criteria.

``` {.haskell #solution-day-11}
repeatM :: (Applicative m) => Int -> m a -> m [a]
repeatM n a = loop n
    where loop n
            | n <= 0    = pure []
            | otherwise = liftA2 (:) a (loop (n - 1))

solutionA :: Array2 Int -> Int
solutionA = sum . evalState (repeatM 100 step)

countRepeatUntilM :: (Monad m) => m Bool -> m Int
countRepeatUntilM action = go 1
    where go n = do
            stop <- action
            if stop then return n else go (n + 1)

solutionB :: Array2 Int -> Int
solutionB = evalState $ countRepeatUntilM ((== 100) <$> step)
```

### Plots
``` {.haskell #show-data-day11 .hide}
printArray2 :: (MonadIO m) => Array2 Int -> m ()
printArray2 a =
    print $ Text.intercalate "\n" $ map (Text.intercalate " " . map tshow) (A.toLists2 a)

print :: (MonadIO m) => Text -> m ()
print = putStr . Text.encodeUtf8

showData :: IO ()
showData = runSimpleApp $ do
    inp <- readInput
    void $ evalStateT (repeatM 258 printStep) inp
    where printStep = step >> get >>= printArray2 >> print "\n\n"
```

``` {.gnuplot output=fig/day11.svg}
Iteration 1 through 256 of the Dumbo Octopusses.
---
set term svg size 500, 500

set tmargin 0.1
set bmargin 0.1
set rmargin 0.1
set lmargin 0.1

unset xtics
unset ytics
unset key
unset colorbox

set size square
set multiplot layout 16,16
do for [t=0:255] {
        plot 'data/day11-output.txt' index t matrix w image pixels t''
}
unset multiplot
```
