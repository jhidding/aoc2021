module Main where

import RIO
import qualified RIO.Map as Map
import Options.Applicative

import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day07
import qualified Day08
import qualified Day09
import qualified Day10
import qualified Day10Alt
import qualified Day11
import qualified Day12
import qualified Day13
import qualified Day14
import qualified Day15
import qualified Day16
import qualified Day17
import qualified Day18
import qualified Day19
import qualified Day20
import qualified Day21
import qualified Day22
import qualified Day23
import qualified Day24

solutions :: Map Text (RIO SimpleApp (), RIO SimpleApp ())
solutions = Map.fromList
    [  ("01", (Day01.runA, Day01.runB))
    ,  ("02", (Day02.runA, Day02.runB))
    ,  ("03", (Day03.runA, Day03.runB))
    ,  ("04", (Day04.runA, Day04.runB))
    ,  ("05", (Day05.runA, Day05.runB))
    ,  ("06", (Day06.runA, Day06.runB))
    ,  ("07", (Day07.runA, Day07.runB))
    ,  ("08", (Day08.runA, Day08.runB))
    ,  ("09", (Day09.runA, Day09.runB))
    , ("10", (Day10.runA, Day10.runB))
    , ("10.1", (Day10Alt.runA, Day10Alt.runB))
    , ("11", (Day11.runA, Day11.runB))
    , ("12", (Day12.runA, Day12.runB))
    , ("13", (Day13.runA, Day13.runB))
    , ("14", (Day14.runA, Day14.runB))
    , ("15", (Day15.runA, Day15.runB))
    , ("16", (Day16.runA, Day16.runB))
    , ("17", (Day17.runA, Day17.runB))
    , ("18", (Day18.runA, Day18.runB))
    , ("19", (Day19.runA, Day19.runB))
    , ("20", (Day20.runA, Day20.runB))
    , ("21", (Day21.runA, Day21.runB))
    , ("22", (Day22.runA, Day22.runB))
    , ("23", (Day23.runA, Day23.runB))
--    , ("24", (Day24.runA, Day24.runB))
    ]

data CommandArgs = CommandArgs
    { dayArg :: Maybe Text
    , runAll :: Bool }

commandArgs :: Parser CommandArgs
commandArgs = CommandArgs <$> optional
                            ( strOption
                            $  long "day" <> short 'd' 
                            <> help "run solution for this day, defaults to latest" )
                          <*> switch ( long "all" <> short 'a' <> help "Run everything" )        

args :: ParserInfo CommandArgs
args = info (commandArgs <**> helper)
            ( fullDesc
            <> progDesc "Run solutions for Advent of Code 2020"
            <> header "x2020 - a puzzle solver")

main :: IO ()
main = runSimpleApp $ do
    args <- liftIO $ execParser args
    if runAll args then
        mapM_ (uncurry runSolution) (Map.toList solutions)
    else fromMaybe (logInfo "No solution for that day") $ do
        d <- day args
        s <- solution d
        Just $ runSolution d s
    where day args = dayArg args <|> (fst <$> Map.lookupMax solutions)
          solution day = (solutions Map.!? day) <|> (snd <$> Map.lookupMax solutions)
          runSolution n (a, b) = logInfo (display $ "Day " <> tshow n <> " ========")
                               >> a >> b >> logInfo ""
