-- ~\~ language=Haskell filename=app/Day02.hs
-- ~\~ begin <<lit/day02.md|app/Day02.hs>>[0]
module Day02 where

import RIO
import RIO.List (foldl)
import qualified RIO.Text as Text
import Parsing (readInputParsing, Parser, lexeme, string, integer, sepEndBy1, eol)

-- ~\~ begin <<lit/day02.md|solution-day-2>>[0]
data Instruction
    = GoForward Int
    | GoUp Int
    | GoDown Int
    deriving (Show)

instructions :: Parser [Instruction]
instructions = sepEndBy1 (lexeme direction <*> integer) eol
    where direction =   (string "forward" $> GoForward)
                    <|> (string "up"      $> GoUp)
                    <|> (string "down"    $> GoDown)

readInput :: (MonadIO m, MonadReader env m, HasLogFunc env) => m [Instruction]
readInput = readInputParsing "data/day02.txt" instructions
-- ~\~ end
-- ~\~ begin <<lit/day02.md|solution-day-2>>[1]
type Pos = (Int, Int)

moveA :: Pos -> Instruction -> Pos
moveA (x, y) (GoForward dx) = (x + dx, y)
moveA (x, y) (GoUp dy)      = (x, y - dy)
moveA (x, y) (GoDown dy)    = (x, y + dy)

solutionA :: [Instruction] -> Int
solutionA inst = x * y
    where (x, y) = foldl moveA (0, 0) inst
-- ~\~ end
-- ~\~ begin <<lit/day02.md|solution-day-2>>[2]
data Navigation = Navigation
    { navDepth :: Int
    , navAim   :: Int
    , navPos   :: Int
    } deriving (Show)

moveB :: Navigation -> Instruction -> Navigation
moveB n@Navigation{..} (GoForward x) = n{ navPos = navPos + x
                                        , navDepth = navDepth + navAim * x }
moveB n@Navigation{..} (GoUp x)      = n{ navAim = navAim - x }
moveB n@Navigation{..} (GoDown x)    = n{ navAim = navAim + x }

solutionB :: [Instruction] -> Int
solutionB inst = navPos * navDepth
    where Navigation{..} = foldl moveB (Navigation 0 0 0) inst
-- ~\~ end
-- ~\~ begin <<lit/boilerplate.md|run-solutions>>[0]
runA :: (HasLogFunc env) => RIO env ()
runA = readInput >>= logInfo . display . solutionA 

runB :: (HasLogFunc env) => RIO env ()
runB = readInput >>= logInfo . display . solutionB
-- ~\~ end
-- ~\~ end
