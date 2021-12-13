# Day 5: Hydrothermal Venture
We need to plot a map of hydrothermal vents on a grid. We are given lists of coordinates in the form `x1,y1 -> x2,y2`. Since we are plotting on 2D grids again, I reach for my friend `Massiv`. Today, we'll see how to program in Haskell like its Fortran 77.

``` {.haskell file=app/Day05.hs}
module Day05 where

import RIO hiding (try)
import RIO.ByteString (putStr)
import qualified RIO.Text as Text
import RIO.List.Partial (foldl1)
import RIO.List (partition, headMaybe, lastMaybe)
import Parsing ( Parser, sepEndBy1, sepBy1, char, hspace, eol
               , integer, lexeme, readInputParsing
               , string )
import qualified Data.Massiv.Array as A
import qualified Data.Massiv.Array.Mutable as MA

<<data-types-day-5>>
<<parser-day-5>>
<<solution-day-5>>
<<extra-day-5>>
<<run-solutions>>
```

``` {.make target=fig/day05-input.svg}
My input data
---
data/day05-plot.txt: app/Day05.hs
    echo -e "import Day05\nshowData\n" | cabal repl -v0 > $@

$(target): export output = $(target)
$(target): export script = load "build/plot-day5-input.gp"
$(target): data/day05-plot.txt build/plot-day5-input.gp
    @echo "plotting day 5 - input data"
    cat templates/gnuplot.preamble | envsubst | gnuplot
```

``` {.gnuplot .hide file=build/plot-day5-input.gp}
set size square
plot 'data/day05-plot.txt' i 0 \
     u 1:2:($3-$1):($4-$2) w vectors t''
```

I like to have position variables that I can treat like applicatives.

``` {.haskell #data-types-day-5}
newtype Vec2 a = Vec2 (a, a)

instance Show a => Show (Vec2 a) where
    show (Vec2 (x, y)) = "(" <> show x <> " " <> show y <> ")"

instance Functor Vec2 where
    fmap f (Vec2 (x, y)) = Vec2 (f x, f y)

instance Applicative Vec2 where
    pure x = Vec2 (x, x)
    liftA2 f (Vec2 (ax, ay)) (Vec2 (bx, by)) = Vec2 (f ax bx, f ay by)

type Pos = Vec2 Int

pos :: Int -> Int -> Pos
pos x y = Vec2 (x, y)

type Line = (Pos, Pos)

makeLine :: Int -> Int -> Int -> Int -> Line
makeLine x1 y1 x2 y2 = (pos x1 y1, pos x2 y2)
```

Now we can parse the list of lines:

``` {.haskell #parser-day-5}
lineP :: Parser Line
lineP = makeLine <$> integer <* lexeme (char ',') <*> integer
                 <*  lexeme (string "->")
                 <*> integer <* lexeme (char ',') <*> integer

readInput :: (HasLogFunc env) => RIO env [Line]
readInput = readInputParsing "data/day05.txt" (sepEndBy1 lineP eol)
```

We need to plot the lines on a diagram. I will be using the `ST` monad to do mutations on the diagram sequentially.

``` {.haskell #data-types-day-5}
type Diagram = A.Array A.P A.Ix2 Int
type MutDiagram s = MA.MArray s A.P A.Ix2 Int
```

We need to know the min/max coordinates of the lines.

``` {.haskell #solution-day-5}
lineMinMax :: Line -> (Pos, Pos)
lineMinMax (a, b) = (min <$> a <*> b, max <$> a <*> b)

totalMinMax :: [Line] -> (Pos, Pos)
totalMinMax ls = foldl1 minMax $ lineMinMax <$> ls
    where minMax (a, b) (c, d) = (min <$> a <*> c, max <$> b <*> d)
```

### Part A
In part A, we only need to treat the lines that are vertical or horizontal. We can write a routine that plots the line on the diagram, given a list of coordinates:

``` {.haskell #solution-day-5}
plotCoords :: MutDiagram s -> [A.Ix2] -> ST s ()
plotCoords d = mapM_ (MA.modify_ d (return . (+ 1)))
```

Now we need to generate the list of coordinates, taking care that origin and end point can be flipped. I make a generic function that splits on several cases:

``` {.haskell #solution-day-5}
range :: Int -> Int -> [Int]
range a b
    | a > b = reverse [b .. a]
    | otherwise = [a .. b]

lineCoords :: Line -> [A.Ix2]
lineCoords l
    <<day-5-line-cases>>
    | otherwise = error $ "Illegal line: " <> show l
```

#### Horizontal
``` {.haskell #day-5-line-cases}
| horizontal l = horizontalCoords l
```

``` {.haskell #solution-day-5}
horizontal :: Line -> Bool
horizontal (Vec2 (_, ay), Vec2 (_, by)) = ay == by

horizontalCoords :: Line -> [A.Ix2]
horizontalCoords (Vec2 (ax, y), Vec2 (bx, _))
    = A.toIx2 . (,y) <$> range ax bx
```

#### Vertical
``` {.haskell #day-5-line-cases}
| vertical l = verticalCoords l
```

``` {.haskell #solution-day-5}
vertical :: Line -> Bool
vertical (Vec2 (ax, _), Vec2 (bx, _)) = ax == bx

verticalCoords :: Line -> [A.Ix2]
verticalCoords (Vec2 (x, ay), Vec2 (_, by))
    = A.toIx2 . (x,) <$> range ay by
```

Now, for the solution:

``` {.haskell #solution-day-5}
plotLines :: [Line] -> Diagram
plotLines l = runST $ do
    arr <- MA.newMArray (A.Sz2 1000 1000) 0
    mapM_ (plotCoords arr . lineCoords) l
    MA.freezeS arr

solutionA :: [Line] -> Int
solutionA = length . filter (> 1) . A.toList 
          . plotLines . filter (not . diagonal)
```

### Part B
Adding the case of diagonal lines:
``` {.haskell #day-5-line-cases}
| diagonal l = diagonalCoords l
```

``` {.haskell #solution-day-5}
diagonal :: Line -> Bool
diagonal (Vec2 (ax, ay), Vec2 (bx, by))
    = abs (ax - bx) == abs (ay - by)

diagonalCoords :: Line -> [A.Ix2]
diagonalCoords (Vec2 (ax, ay), Vec2 (bx, by))
    = A.toIx2 <$> zip (range ax bx) (range ay by)

solutionB :: [Line] -> Int
solutionB = length . filter (> 1) . A.toList . plotLines
```

``` {.haskell #extra-day-5 .hide}
showData :: IO ()
showData = runSimpleApp $ do
    inp <- readInput
    let result = plotLines inp
    mapM_ (\(Vec2 (ax, ay), Vec2 (bx, by))
           -> print $ tshow ax <> " " <> tshow ay <> " " <> tshow bx <> " " <> tshow by <> "\n")
          inp
    print "\n\n"
    print $ Text.intercalate "\n" $ map (Text.intercalate " " . map tshow) (A.toLists2 result)
    return ()
    where print = putStr . Text.encodeUtf8
```

``` {.make target=fig/day05-output.svg}
My output data
---
data/day05-plot.txt: app/Day05.hs
    echo -e "import Day05\nshowData\n" | cabal repl -v0 > $@

$(target): export output = $(target)
$(target): export script = load "build/plot-day5-output.gp"
$(target): data/day05-plot.txt build/plot-day5-output.gp
    cat templates/gnuplot.preamble | envsubst | gnuplot
```

``` {.gnuplot .hide file=build/plot-day5-output.gp}
set size square
set xrange [0:1000]
set yrange [0:1000]
plot 'data/day05-plot.txt' i 1 matrix w image t''
```
