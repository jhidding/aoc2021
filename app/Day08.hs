-- ~\~ language=Haskell filename=app/Day08.hs
-- ~\~ begin <<lit/day08.md|app/Day08.hs>>[0]
module Day08 where

import RIO
import RIO.List (foldl, find)
import Data.Map.Lazy (Map, (!?))
import qualified Data.Map.Lazy as Map
import Data.Tuple (swap)
import qualified RIO.Set as Set
import qualified RIO.Text as Text
import Parsing ( Parser, readInputParsing, sepEndBy1
               , lexeme, char, eol)
import Text.Megaparsec (takeWhile1P)

-- ~\~ begin <<lit/day08.md|data-types-day-8>>[0]
newtype Digit = Digit { digitSet :: Set Char }
    deriving (Show, Ord, Eq)

instance Semigroup Digit where
    Digit a <> Digit b = Digit $ (a Set.\\ b) `Set.union` (b Set.\\ a)

instance Monoid Digit where
    mempty = Digit mempty

eight :: Digit
eight = Digit $ Set.fromList ['a'..'g']

(<<<) :: Digit -> Digit -> Bool
Digit a <<< Digit b = a `Set.isSubsetOf` b

(\\) :: Digit -> Digit -> Digit
Digit a \\ Digit b = Digit $ a Set.\\ b

invert :: Digit -> Digit
invert = (eight \\)

numberOfSegments :: Digit -> Int
numberOfSegments (Digit a) = Set.size a

data Line = Line
    { signalPattern :: [Digit]
    , outputValues  :: [Digit]
    } deriving (Show)
-- ~\~ end
-- ~\~ begin <<lit/day08.md|parser-day-8>>[0]
word :: Parser Text
word = lexeme $ takeWhile1P (Just "letter a-g") (\c -> c >= 'a' && c <= 'g')

charSet :: Parser Digit
charSet = Digit . Set.fromList . Text.unpack <$> word

lineP :: Parser Line
lineP = Line <$> some charSet <* lexeme (char '|') <*> some charSet

readInput :: (HasLogFunc env) => RIO env [Line]
readInput = readInputParsing "data/day08.txt" (sepEndBy1 lineP eol)
-- ~\~ end
-- ~\~ begin <<lit/day08.md|solution-day-8>>[0]
solutionA :: [Line] -> Int
solutionA = length . filter ((`elem` [2, 3, 4, 7]) . numberOfSegments)
                   . concatMap outputValues
-- ~\~ end
-- ~\~ begin <<lit/day08.md|solution-day-8>>[1]
type Decoding = Map Digit Int

invertMap :: (Ord k, Ord v) => Map k v -> Map v k
invertMap = Map.fromList . map swap . Map.toList

generateMap :: (Ord k) => (k -> v) -> [k] -> Map k v
generateMap f = Map.fromList . map (\k -> (k, f k))

decode :: [Digit] -> Decoding
decode digits = invertMap $ Map.mapMaybe id decodedMap
    where decodedMap = generateMap (\i -> find (match i) digits) [0..9]
          getDigit = join . (decodedMap !?)
          match i digit
              -- ~\~ begin <<lit/day08.md|digit-decode-cases>>[0]
              | i == 1             = l == 2
              | i == 4             = l == 4
              | i == 7             = l == 3
              | i == 8             = l == 7
              | i `elem` [0, 6, 9] = l == 6 && 
                  -- ~\~ begin <<lit/day08.md|digit-6-segments>>[0]
                  fromMaybe False ( do
                    one  <- getDigit 1
                    four <- getDigit 4
                    return $ i == 0 && invert digit <<< four
                                    && one <<< digit
                          || i == 6 && invert digit <<< one
                          || i == 9 && four <<< digit )
                  -- ~\~ end
              | i `elem` [2, 3, 5] = l == 5 &&
                  -- ~\~ begin <<lit/day08.md|digit-5-segments>>[0]
                  fromMaybe False ( do
                    one <- getDigit 1
                    six <- getDigit 6
                    return $ i == 3 && one <<< digit
                          || i == 2 && invert six <<< digit
                                    && not (one <<< digit)
                          || i == 5 && not (invert six <<< digit)
                                    && not (one <<< digit) )
                  -- ~\~ end
              -- ~\~ end
              | otherwise            = False
              where l = numberOfSegments digit
-- ~\~ end
-- ~\~ begin <<lit/day08.md|solution-day-8>>[2]
decodeLine :: Line -> Int
decodeLine Line{..} = fromDecimal $ mapMaybe (d !?) outputValues
    where d = decode signalPattern
          fromDecimal = foldl (\a b -> a * 10 + b) 0

solutionB :: [Line] -> Int
solutionB = sum . map decodeLine
-- ~\~ end
-- ~\~ begin <<lit/boilerplate.md|run-solutions>>[0]
runA :: (HasLogFunc env) => RIO env ()
runA = readInput >>= logInfo . display . tshow . solutionA 

runB :: (HasLogFunc env) => RIO env ()
runB = readInput >>= logInfo . display . tshow . solutionB
-- ~\~ end
-- ~\~ end
