# Day 17: Trick Shot
Today we need to do some math. The first part, we can even compute by hand! We are given a target area for a probe. Starting at position $(0,0)$, and an unknown initial velocity, we are given a rectangular area to hit. The probe lives in the weird integer arithmetic universe that we've come to love from Advent of Code

``` {.haskell file=app/Day17.hs}
module Day17 where

import RIO
import Parsing (Parser, readInputParsing, string, integer, lexeme, char)
import Linear.V2 (V2(..))
import Print ( printLn )

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

The rules are that each timestep:

* position increases with velocity
* the velocity in x-direction decreases in magnitude due to drag
* the velocity in y-direction increases in negative direction by one due to gravity

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
so the minimum velocity is $\lfloor \sqrt{2 x_{\rm min}} \rfloor$. The maximum velocity is $x_{\rm max}$.

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

``` {.haskell .hide #solution-day17}
iterateUntil' :: (a -> a) -> (a -> Bool) -> a -> [a]
iterateUntil' f p init
    | p init = [init]
    | otherwise = init : iterateUntil' f p (f init)

showData :: IO ()
showData = do
    area <- runSimpleApp readInput
    let (V2 minvx minvy, V2 maxvx maxvy) = velocityBounds area
        startVels = [ V2 vx vy
                           | vx <- [minvx .. maxvx]
                           , vy <- [minvy .. maxvy] ]
    mapM_ (printStats area) startVels
    printLn "\n"
    printLn $ "# " <> tshow (length startVels)
    mapM_ (printShot area) startVels
    where printShot area v = do
            let o = if outcome area v == Hit then 1 else 0
            let path = iterateUntil' step (\x -> hit area x || miss area x) 
                                (PhaseSpace (V2 0 0) v)
            when (length path > 10) (mapM_ (printPs o) path >> printLn "\n")
          printPs o (PhaseSpace (V2 x y) (V2 vx vy)) = printLn 
            $ tshow x <> " " <> tshow y <> " " <> tshow vx <> " "
            <> tshow vy <> " " <> tshow o
          printStats area v@(V2 vx vy) = do
            let o = if outcome area v == Hit then 1 else 0
            let l = if o == 1
                    then length $ iterateUntil' step (\x -> hit area x || miss area x) 
                                (PhaseSpace (V2 0 0) v)
                    else -1
            when (o == 1) $
                printLn $ tshow vx <> " " <> tshow vy <> " " <> tshow o <> " " <> tshow l
```

If I plot the time of impact for every initial velocity, a structure to the solution appears, which makes me think there should be a nicer solution to this problem than brute forcing.

``` {.gnuplot .hide file=build/plot-day17.gp}
set term svg size 800, 800
set size ratio -1
set xrange [10:205]
set yrange [-120:120]
plot 'data/day17-output.txt' i 0 u 1:2:4 w p lc palette pt 7 ps 0.5 t''
```

``` {.make target=fig/day17-code.svg}
$(target): export output=$(target)
$(target): export script=load "build/plot-day17.gp"
$(target): build/plot-day17.gp data/day17-output.txt
    cat templates/gnuplot.preamble | envsubst | gnuplot
```

We have an initial set of trivial solutions, reaching the target area in one time step. From that we may be able to derive a set of solutions that reach the same target in two steps, and so on.

Given a point $p = (p_x, p_y)$, we may reach this point in one step if the initial velocity $v(0) = p$. We can compute the effect of two time steps.

$$x(2) = v_x(0) + v_x(1) = 2 v_x(0) - 1$$
$$y(2) = v_y(0) + v_y(1) = 2 v_y(0) + 1$$

In general we can say,

$$x(t) = \min (t * v_x(0) - \Delta(t - 1), \Delta(v_x(0))),$$
$$y(t) = t * v_y(0) - \Delta(t - 1),$$

where $\Delta(t) = t(t+1)/2.$ Now the question is, can we invert those to solve $v(0)$ from $x(t)$? The $y$ component is not too hard:

$$v_y(0) = (y(t) + \Delta(t - 1)) / t = y(t) / t + (t - 1) / 2,$$

noting that we're still limited to integer solutions; $y(t) \mod t = 0$ if $t$ is odd and $y(t) \mod t = t/2$ if $t$ is even.

The $x$ velocity is a bit more tricky. If $t \le v_x(0)$, then the equation is the same as for $y$. If $t > v_x(0)$ then

$$x(t) = v_x(0) (v_x(0) + 1) / 2,$$

So the equation can only be solved if $x(t)$ is a triangular number, and then,

$$v_x(0) = \lfloor \sqrt{2 x(t)} \rfloor.$$

We can plot the resulting boxes for each time $t$.

``` {.haskell #solution-day17}
invertArea :: Area -> Int -> Area
invertArea Area{..} t = Area minX' maxX' minY' maxY'
    where invertDelta x = floor (sqrt (fromIntegral $ 2 * x))
          invertQuadratic :: Int -> Float
          invertQuadratic x = fromIntegral x / fromIntegral t 
                            + (fromIntegral t - 1) / 2
          minX' = max (invertDelta minX) (ceiling $ invertQuadratic minX)
          maxX' = floor (invertQuadratic maxX)
          minY' = ceiling (invertQuadratic minY)
          maxY' = floor (invertQuadratic maxY)

printArea :: Area -> IO ()
printArea Area{..} = do
    printLn $ tshow minX <> " " <> tshow minY
    printLn $ tshow maxX <> " " <> tshow minY
    printLn $ tshow maxX <> " " <> tshow maxY
    printLn $ tshow minX <> " " <> tshow maxY
    printLn $ tshow minX <> " " <> tshow minY

showData2 :: IO ()
showData2 = do
    area <- runSimpleApp readInput
    mapM_ (\t -> printArea (invertArea area t) >> printLn "\n")
          [0 .. 2 * negate (minY area)]
```

