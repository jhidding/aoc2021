-- ~\~ language=Haskell filename=app/Day13.hs
-- ~\~ begin <<lit/day13.md|app/Day13.hs>>[0]
module Day13 where

import RIO
import RIO.ByteString (putStr)
import RIO.List.Partial (head)
import qualified RIO.Text as Text
import qualified RIO.Set as Set
import Parsing (readInputParsing, Parser, string, sepEndBy1, eol, integer, char)
import Data.Massiv.Array (Ix2(..))

-- ~\~ begin <<lit/day13.md|parser-day-13>>[0]
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
-- ~\~ end
-- ~\~ begin <<lit/day13.md|solution-day-13>>[0]
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
-- ~\~ end
-- ~\~ begin <<lit/day13.md|solution-day-13>>[1]
foldAllFolds :: Input -> [Ix2]
foldAllFolds Input{..} = Set.toList $ foldl' makeFold 
                                             (Set.fromList inputCoordinates)
                                             foldInstructions
    where makeFold s i = Set.map (foldTransform i) s
-- ~\~ end
-- ~\~ begin <<lit/day13.md|solution-day-13>>[2]
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
-- ~\~ end
-- ~\~ end
