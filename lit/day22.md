# Day 22: Reactor Reboot
For today's puzzle it is immediately clear what the second part is about. We need to find the number of lights turned on in a crazy large space.

``` {.haskell file=app/Day22.hs}
module Day22 where

import RIO
import qualified RIO.Map as Map
import Linear.V3 (V3(..), _x, _y, _z)
import Parsing
    ( Parser, integer, lexeme, readInputParsing, eol, string, sepEndBy1 )

<<parser-day22>>
<<solution-day22>>
<<run-solutions>>
```

Obligatory parser of the day:

``` {.haskell #parser-day22}
type V3Range = (V3 Int, V3 Int)
data Command = CommandOn | CommandOff deriving (Show, Eq, Ord, Bounded, Enum)
type Input = [(Command, V3Range)]

intRangeP :: Parser (Int, Int)
intRangeP = (,) <$> integer <* string ".." <*> integer

rangeP :: Parser (Command, V3Range)
rangeP = (,) <$> lexeme (CommandOn <$ string "on" <|> (CommandOff <$ string "off"))
             <*> (do (xmin, xmax) <- string "x=" *> intRangeP
                     string ","
                     (ymin, ymax) <- string "y=" *> intRangeP
                     string ","
                     (zmin, zmax) <- string "z=" *> intRangeP
                     return (V3 xmin ymin zmin, V3 xmax ymax zmax))

inputP :: Parser Input
inputP = rangeP `sepEndBy1` eol

readInput :: (HasLogFunc env) => RIO env Input
readInput = readInputParsing "data/day22.txt" inputP
```

I define a class `Range` on which an `intersection` and `area` are defined. This has a rather straight forward implementation on `(Int, Int)`.

``` {.haskell #solution-day22}
class Range a where
    intersect :: a -> a -> Maybe a
    area :: a -> Int

instance Range (Int, Int) where
    intersect (a1, a2) (b1, b2)
        | b1 > a2 || a1 > b2 = Nothing
        | otherwise          = Just (max a1 b1, min a2 b2)

    area (a1, a2) = a2 - a1 + 1
```

Now, for `(V3 Int, V3 Int)` it is just the combination of the integer intersections.

``` {.haskell #solution-day22}
instance Range (V3 Int, V3 Int) where
    intersect (a1, a2) (b1, b2) = do
        (x1, x2) <- (a1 ^. _x, a2 ^. _x) `intersect` (b1 ^. _x, b2 ^. _x)
        (y1, y2) <- (a1 ^. _y, a2 ^. _y) `intersect` (b1 ^. _y, b2 ^. _y)
        (z1, z2) <- (a1 ^. _z, a2 ^. _z) `intersect` (b1 ^. _z, b2 ^. _z)
        return (V3 x1 y1 z1, V3 x2 y2 z2)

    area (a, b) = product (b - a + 1)
```

Now a `[(r, Int)]` type is giving a list of ranges and their multiplicity. If a range of lights is turned off, we find the intersection with all current ranges and stack those with opposite sign.

``` {.haskell #solution-day22}
(=-=) :: Range r => [(r, Int)] -> r -> [(r, Int)]
m =-= r = mapMaybe isect m <> m
    where isect (s, m) = (, negate m) <$> (s `intersect` r)
```

To switch lights on, first switch them off, and then add to the list.

``` {.haskell #solution-day22}
(=+=) :: Range r => [(r, Int)] -> r -> [(r, Int)]
m =+= r = (r, 1) : (m =-= r)

runCommands :: Input -> Int
runCommands = totalArea . foldl' switch []
    where switch m (CommandOn, r) = m =+= r
          switch m (CommandOff, r) = m =-= r
          totalArea = sum . map (\(r, s) -> area r * s)

solutionA :: Input -> Int
solutionA = runCommands . filter (small . snd)
    where small (a, b) = all ((<=50) . abs) a && all ((<=50) . abs) b

solutionB :: Input -> Int
solutionB = runCommands
```
