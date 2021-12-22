-- ~\~ language=Haskell filename=app/Day22.hs
-- ~\~ begin <<lit/day22.md|app/Day22.hs>>[0]
module Day22 where

import RIO
import qualified RIO.Map as Map
import Linear.V3 (V3(..), _x, _y, _z)
import Parsing
    ( Parser, integer, lexeme, readInputParsing, eol, string, sepEndBy1 )

-- ~\~ begin <<lit/day22.md|parser-day22>>[0]
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
-- ~\~ end
-- ~\~ begin <<lit/day22.md|solution-day22>>[0]
class Range a where
    intersect :: a -> a -> Maybe a
    area :: a -> Int

instance Range (Int, Int) where
    intersect (a1, a2) (b1, b2)
        | b1 > a2 || a1 > b2 = Nothing
        | otherwise          = Just (max a1 b1, min a2 b2)

    area (a1, a2) = a2 - a1 + 1
-- ~\~ end
-- ~\~ begin <<lit/day22.md|solution-day22>>[1]
instance Range (V3 Int, V3 Int) where
    intersect (a1, a2) (b1, b2) = do
        (x1, x2) <- (a1 ^. _x, a2 ^. _x) `intersect` (b1 ^. _x, b2 ^. _x)
        (y1, y2) <- (a1 ^. _y, a2 ^. _y) `intersect` (b1 ^. _y, b2 ^. _y)
        (z1, z2) <- (a1 ^. _z, a2 ^. _z) `intersect` (b1 ^. _z, b2 ^. _z)
        return (V3 x1 y1 z1, V3 x2 y2 z2)

    area (a, b) = product (b - a + 1)
-- ~\~ end
-- ~\~ begin <<lit/day22.md|solution-day22>>[2]
newtype MultiRange r = MultiRange { toList :: [(r, Int)] }
    deriving (Show, Semigroup, Monoid)

(=-=) :: Range r => MultiRange r -> r -> MultiRange r
(MultiRange m) =-= r = MultiRange $ mapMaybe isect m <> m
    where isect (s, m) = (, negate m) <$> (s `intersect` r)
-- ~\~ end
-- ~\~ begin <<lit/day22.md|solution-day22>>[3]
(=+=) :: Range r => MultiRange r -> r -> MultiRange r
m =+= r = (m =-= r) <> MultiRange [(r, 1)]

runCommands :: Input -> Int
runCommands = totalArea . foldl' switch (MultiRange []) 
    where switch m (CommandOn, r) = m =+= r
          switch m (CommandOff, r) = m =-= r
          totalArea (MultiRange m) = foldl' (\t (r, s) -> t + area r * s) 0 m

solutionA :: Input -> Int
solutionA = runCommands . filter (small . snd)
    where small (a, b) = all ((<=50) . abs) a && all ((<=50) . abs) b

solutionB :: Input -> Int
solutionB = runCommands
-- ~\~ end
-- ~\~ begin <<lit/boilerplate.md|run-solutions>>[0]
runA :: (HasLogFunc env) => RIO env ()
runA = readInput >>= logInfo . display . tshow . solutionA

runB :: (HasLogFunc env) => RIO env ()
runB = readInput >>= logInfo . display . tshow . solutionB
-- ~\~ end
-- ~\~ end
