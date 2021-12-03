-- ~\~ language=Haskell filename=app/Day01.hs
-- ~\~ begin <<lit/day01.md|app/Day01.hs>>[0]
module Day01 where

import RIO
import qualified RIO.Text as Text

readInput :: MonadIO m => m [Int]
readInput = do
    text <- Text.lines <$> readFileUtf8 "data/day01.txt"
    return $ mapMaybe (readMaybe . Text.unpack) text

-- ~\~ begin <<lit/day01.md|solution-day-1>>[0]
solutionA :: [Int] -> Int
solutionA = length . filter (> 0) . diff
    where diff (a1:a2:as) = a2 - a1 : diff (a2:as)
          diff _          = []
-- ~\~ end
-- ~\~ begin <<lit/day01.md|solution-day-1>>[1]
solutionB :: [Int] -> Int
solutionB = solutionA . slidingSum
    where slidingSum (a1:a2:a3:as) = a1 + a2 + a3 : slidingSum (a2:a3:as)
          slidingSum _             = []
-- ~\~ end
-- ~\~ begin <<lit/boilerplate.md|run-solutions>>[0]
runA :: (HasLogFunc env) => RIO env ()
runA = readInput >>= logInfo . display . solutionA 

runB :: (HasLogFunc env) => RIO env ()
runB = readInput >>= logInfo . display . solutionB
-- ~\~ end
-- ~\~ end
