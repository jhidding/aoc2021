-- ~\~ language=Haskell filename=app/Day10Alt.hs
-- ~\~ begin <<lit/day10.md|app/Day10Alt.hs>>[0]
module Day10Alt where

import RIO

import RIO.List.Partial ((!!))
import RIO.List (sort, headMaybe, foldl)

import qualified RIO.Text as Text
import RIO.ByteString (readFile)

readInput :: (MonadIO m) => m [Text]
readInput = Text.lines . Text.decodeUtf8With Text.lenientDecode 
         <$> readFile "data/day10.txt"

data ParseResult = Unexpected Char | AutoComplete Text | Success Text

parse :: Text -> ParseResult
parse inp = go (Text.unpack inp) []
    where go [] []      = Success inp
          go [] exp     = AutoComplete (Text.pack exp)
          go (c:cs) exp = fromMaybe (Unexpected c) 
                                    (close (c:cs) exp <|> open (c:cs) exp)

          close (c:cs) (e:exp)
            | c == e    = Just $ go cs exp
            | otherwise = Nothing
          close (c:cs) [] = Nothing

          open (c:cs) exp
            | c == '('  = Just $ go cs (')':exp)
            | c == '['  = Just $ go cs (']':exp)
            | c == '<'  = Just $ go cs ('>':exp)
            | c == '{'  = Just $ go cs ('}':exp)
            | otherwise = Nothing

solutionA :: [Text] -> Int
solutionA = sum . map (score . parse)
    where score (Unexpected ')') = 3
          score (Unexpected ']') = 57
          score (Unexpected '}') = 1197
          score (Unexpected '>') = 25137
          score _                = 0

-- ~\~ begin <<lit/day10.md|median>>[0]
median :: [Int] -> Int
median x = sort x !! (length x `div` 2)
-- ~\~ end

solutionB :: [Text] -> Int
solutionB = median . mapMaybe (score . parse)
    where score (AutoComplete t) = Just $ foldl (\i c -> i * 5 + points c) 0 (Text.unpack t)
          score _                = Nothing
          points ')' = 1
          points ']' = 2
          points '}' = 3
          points '>' = 4
          points _   = 0

-- ~\~ begin <<lit/boilerplate.md|run-solutions>>[0]
runA :: (HasLogFunc env) => RIO env ()
runA = readInput >>= logInfo . display . tshow . solutionA

runB :: (HasLogFunc env) => RIO env ()
runB = readInput >>= logInfo . display . tshow . solutionB
-- ~\~ end
-- ~\~ end
