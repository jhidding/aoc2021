# Day 17: Trick Shot
Today we need to do some math. The first part, we can even compute by hand! We are given a target area for a probe. Starting at position $(0,0)$, and an unknown initial velocity, we are given a rectangular area to hit. The probe lives in the weird integer arithmetic universe that we've come to love from Advent of Code

``` {.haskell file=app/Day17.hs}
module Day17 where

import RIO
import Parsing (Parser, readInputParsing, string, integer, lexeme, char)
import Linear.V2 (V2(..))

<<data-types-day17>>
<<parser-day17>>
<<solution-day17>>
<<run-solutions>>
```

``` {.haskell #data-types-day17}
data Area = Area
    { minX :: Int
    , maxX :: Int
    , minY :: Int
    , maxY :: Int
    } deriving (Show)

data PhaseSpace = PhaseSpace
    { position :: V2 Int
    , velocity :: V2 Int
    } deriving (Show)
```

But first, parsing! (I know, overkill)

``` {.haskell #parser-day17}
areaP :: Parser Area
areaP = Area <$ string "x=" <*> integer <* rangeSep <*> integer
             <* listSep
             <* string "y=" <*> integer <* rangeSep <*> integer
    where rangeSep = lexeme (string "..")
          listSep = lexeme (char ',')

readInput :: (HasLogFunc env) => RIO env Area
readInput = readInputParsing "data/day17.txt" (string "target area: " *> areaP)
```

The rules are that each timestep, the velocity in x-direction decreases due to drag and the velocity in y-direction increases in negative direction by one.

``` {.haskell #solution-day17}
step :: PhaseSpace -> PhaseSpace
step (PhaseSpace position velocity@(V2 vx vy)) = PhaseSpace
    { position = position + velocity
    , velocity = V2 (vx - signum vx) (vy - 1)
    }
```

We need to see if the probe hits the target area, but also if it definitely missed it.

``` {.haskell #solution-day17}
hit :: Area -> PhaseSpace -> Bool
hit Area{..} (PhaseSpace (V2 x y) _) = minX <= x && x <= maxX
                                    && minY <= y && y <= maxY

miss :: Area -> PhaseSpace -> Bool
miss Area{..} (PhaseSpace (V2 _ y) (V2 _ dy)) = y < minY && dy < 0
```

The key is now to find the maximum velocity upward. The point being that the probe always returns to level 0, with negative that velocity. If that velocity will make the probe overshoot, than we definetly miss target. The minimum velocity is $y_{\rm min}$, so the maximum velocity is $-y_{\rm min} - 1$.

The height attained at the maximum y velocity is $(v_y (v_y + 1)) / 2$.

For the velocity in the X direction, the final X position we reach is $(v_x (v_x + 1))/2$,
so the minimum velocity is $\lfloor \sqrt{x_{\rm min} * 2} \rfloor$. The maximum velocity is $x_{\rm max}$.

``` {.haskell #solution-day17}
velocityBounds :: Area -> (V2 Int, V2 Int)
velocityBounds Area{..} = (V2 minvx minvy, V2 maxvx maxvy)
    where minvy = minY
          maxvy = (-minY) - 1
          minvx = floor (sqrt (fromIntegral $ minX * 2))
          maxvx = maxX

data Outcome = Hit | Miss deriving (Eq)

iterateUntil :: (a -> a) -> (a -> Bool) -> a -> a
iterateUntil f p init
    | p init    = init
    | otherwise = iterateUntil f p (f init)

outcome :: Area -> V2 Int -> Outcome
outcome a v = if hit a last then Hit else Miss
    where last = iterateUntil step (\x -> hit a x || miss a x) 
                              (PhaseSpace (V2 0 0) v)
```

For part B we actually need to compute.

``` {.haskell #solution-day17}
solutionA :: Area -> Int
solutionA Area{..} = maxv * (maxv + 1) `div` 2
    where maxv = (- minY) - 1

solutionB :: Area -> Int
solutionB a = length [ V2 vx vy
                     | vx <- [minvx .. maxvx]
                     , vy <- [minvy .. maxvy]
                     , outcome a (V2 vx vy) == Hit ]
    where (V2 minvx minvy, V2 maxvx maxvy) = velocityBounds a
```
