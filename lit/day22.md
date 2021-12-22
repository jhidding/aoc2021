# Day 22: Reactor Reboot

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

``` {.haskell #parser-day22}
type V3Range = (V3 Int, V3 Int)
data Command = CommandOn | CommandOff deriving (Show, Eq, Ord, Bounded, Enum)
type Input = [(Command, V3Range)]

intRangeP :: Parser (Int, Int)
intRangeP = (,) <$> integer <* string ".." <*> integer

rangeP :: Parser (Command, V3Range)
rangeP = (,) <$> lexeme (CommandOn <$ string "on" <|> (CommandOff <$ string "off"))
             <*> (do
                    (xmin, xmax) <- string "x=" *> intRangeP
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

``` {.haskell #solution-day22}
class Range a where
    intersect :: a -> a -> Maybe a

(=/=) :: (Range a) => a -> a -> Maybe a
(=/=) = intersect

instance Range (Int, Int) where
    intersect (a1, a2) (b1, b2)
        | b1 > a2 || a1 > b2 = Nothing
        | b1 >= a1 && b2 >= a2 = Just (b1, a2)
        | a1 >= b1 && a2 >= b2 = Just (a1, b2)
        | b1 >= a1 && b2 <= a2 = Just (b1, b2)
        | a1 >= b1 && a2 <= b2 = Just (a1, a2)
        | otherwise = error $ "no known configuration: " <> show ((a1, a2), (b1, b2))

instance Range (V3 Int, V3 Int) where
    intersect (a1, a2) (b1, b2) = do
        (x1, x2) <- intersect (a1 ^. _x, a2 ^. _x) (b1 ^. _x, b2 ^. _x)
        (y1, y2) <- intersect (a1 ^. _y, a2 ^. _y) (b1 ^. _y, b2 ^. _y)
        (z1, z2) <- intersect (a1 ^. _z, a2 ^. _z) (b1 ^. _z, b2 ^. _z)
        return (V3 x1 y1 z1, V3 x2 y2 z2)

newtype MultiRange r = MultiRange { toList :: [(r, Int)] }
    deriving (Show)

(=+=) :: Range r => MultiRange r -> r -> MultiRange r
(MultiRange m) =+= r = MultiRange $ (r, 1) : (mapMaybe isect m <> m)
    where isect (s, m) = (, negate m) <$> (s =/= r)

(=-=) :: Range r => MultiRange r -> r -> MultiRange r
(MultiRange m) =-= r = MultiRange $ mapMaybe isect m <> m
    where isect (s, m) = (, negate m) <$> (s =/= r)

solutionA :: Input -> MultiRange (V3 Int, V3 Int)
solutionA = foldl' switch (MultiRange []) . filter (small . snd)
    where switch m (CommandOn, r) = m =+= r
          switch m (CommandOff, r) = m =-= r
          small (a, b) = all ((<=50) . abs) a && all ((<=50) . abs) b

solutionB :: b -> Int
solutionB = const 0
```
