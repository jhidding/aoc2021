# Day 13: Transparent Origami
We need to fold a piece of transparent paper with dots on it.

``` {.haskell file=app/Day13.hs}
module Day13 where

import RIO
import RIO.ByteString (putStr)
import RIO.List.Partial (head)
import qualified RIO.Text as Text
import qualified RIO.Set as Set
import Parsing (readInputParsing, Parser, string, sepEndBy1, eol, integer, char)
import Data.Massiv.Array (Ix2(..))

<<parser-day-13>>
<<solution-day-13>>
```

As always, we have a parser:

``` {.haskell #parser-day-13}
data Input = Input
    { inputCoordinates :: [Ix2]
    , foldInstructions :: [FoldInstruction] }
    deriving (Show)

data FoldInstruction = FoldInstruction Axis Int
    deriving (Show)

data Axis = XAxis | YAxis deriving (Show)

inputP :: Parser Input
inputP = Input <$> coordinatesP <* eol <*> foldInstructionsP

coordinatesP :: Parser [Ix2]
coordinatesP = sepEndBy1 ((:.) <$> integer <* char ',' <*> integer) eol

foldInstructionsP :: Parser [FoldInstruction]
foldInstructionsP = sepEndBy1 foldInstructionP eol
    where foldInstructionP = string "fold along " $> FoldInstruction
                          <*> axisP <* char '=' <*> integer
          axisP =  (XAxis <$ char 'x')
               <|> (YAxis <$ char 'y')

readInput :: (HasLogFunc env) => RIO env Input
readInput = readInputParsing "data/day13.txt" inputP
```

For each fold we need to transform the coordinates.

``` {.haskell #solution-day-13}
foldTransform :: FoldInstruction -> Ix2 -> Ix2
foldTransform (FoldInstruction XAxis loc) (x :. y)
  | x > loc   = 2 * loc - x :. y
  | otherwise = x :. y
foldTransform (FoldInstruction YAxis loc) (x :. y)
  | y > loc   = x :. 2 * loc - y
  | otherwise = x :. y

solutionA :: Input -> Int
solutionA Input{..} = Set.size
                    $ Set.map (foldTransform $ head foldInstructions)
                    $ Set.fromList inputCoordinates
```

Now we need to fold the folds.

``` {.haskell #solution-day-13}
foldAllFolds :: Input -> [Ix2]
foldAllFolds Input{..} = Set.toList $ foldl' makeFold 
                                             (Set.fromList inputCoordinates)
                                             foldInstructions
    where makeFold s i = Set.map (foldTransform i) s
```

Apparently the answer is in visualizing the result, so I'll print out the coordinates and plot them with Gnuplot.

``` {.haskell #solution-day-13}
print :: (MonadIO m) => Text -> m ()
print = putStr . Text.encodeUtf8

printLn :: (MonadIO m) => Text -> m ()
printLn = print . (<> "\n") 

printCoords :: MonadIO m => [Ix2] -> m ()
printCoords = mapM_ (\(x :. y) -> printLn $ tshow x <> " " <> tshow y)

runA :: (HasLogFunc env) => RIO env ()
runA = do
    inp <- readInput
    printLn $ "# " <> tshow (solutionA inp)

runB :: (HasLogFunc env) => RIO env ()
runB = do
    inp <- readInput
    printCoords (foldAllFolds inp)
```

``` {.gnuplot .hide file=build/plot-day13.gp}
set term svg size 800, 200
set yrange [6:-1] reverse
set xrange [-1:40]
set size ratio -1
plot 'data/day13-output.txt' w p pt 7 ps 2 t''
```

``` {.make target=fig/day13-code.svg}
$(target): export output=$(target)
$(target): export script=load "build/plot-day13.gp"
$(target): build/plot-day13.gp data/day13-output.txt
    cat templates/gnuplot.preamble | envsubst | gnuplot
```

