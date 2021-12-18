-- ~\~ language=Haskell filename=app/Day17.hs
-- ~\~ begin <<lit/day17.md|app/Day17.hs>>[0]
module Day17 where

import RIO
import Parsing (Parser, readInputParsing, string, integer, lexeme, char)
import Linear.V2 (V2(..))

-- ~\~ begin <<lit/day17.md|data-types-day17>>[0]
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
-- ~\~ end
-- ~\~ begin <<lit/day17.md|parser-day17>>[0]
areaP :: Parser Area
areaP = Area <$ string "x=" <*> integer <* rangeSep <*> integer
             <* listSep
             <* string "y=" <*> integer <* rangeSep <*> integer
    where rangeSep = lexeme (string "..")
          listSep = lexeme (char ',')

readInput :: (HasLogFunc env) => RIO env Area
readInput = readInputParsing "data/day17.txt" (string "target area: " *> areaP)
-- ~\~ end
-- ~\~ begin <<lit/day17.md|solution-day17>>[0]
step :: PhaseSpace -> PhaseSpace
step (PhaseSpace position velocity@(V2 vx vy)) = PhaseSpace
    { position = position + velocity
    , velocity = V2 (vx - signum vx) (vy - 1)
    }
-- ~\~ end
-- ~\~ begin <<lit/day17.md|solution-day17>>[1]
hit :: Area -> PhaseSpace -> Bool
hit Area{..} (PhaseSpace (V2 x y) _) = minX <= x && x <= maxX
                                    && minY <= y && y <= maxY

miss :: Area -> PhaseSpace -> Bool
miss Area{..} (PhaseSpace (V2 _ y) (V2 _ dy)) = y < minY && dy < 0
-- ~\~ end
-- ~\~ begin <<lit/day17.md|solution-day17>>[2]
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
-- ~\~ end
-- ~\~ begin <<lit/day17.md|solution-day17>>[3]
solutionA :: Area -> Int
solutionA Area{..} = maxv * (maxv + 1) `div` 2
    where maxv = (- minY) - 1

solutionB :: Area -> Int
solutionB a = length [ V2 vx vy
                     | vx <- [minvx .. maxvx]
                     , vy <- [minvy .. maxvy]
                     , outcome a (V2 vx vy) == Hit ]
    where (V2 minvx minvy, V2 maxvx maxvy) = velocityBounds a
-- ~\~ end
-- ~\~ begin <<lit/boilerplate.md|run-solutions>>[0]
runA :: (HasLogFunc env) => RIO env ()
runA = readInput >>= logInfo . display . tshow . solutionA 

runB :: (HasLogFunc env) => RIO env ()
runB = readInput >>= logInfo . display . tshow . solutionB
-- ~\~ end
-- ~\~ end
