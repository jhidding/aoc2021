# Day 8: Seven Segment Search
Oh boy. This was a really nice puzzle. I think I managed to put the solution into readable code also.

``` {.haskell file=app/Day08.hs}
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

<<data-types-day-8>>
<<parser-day-8>>
<<solution-day-8>>
<<run-solutions>>
```

I'll define a `Digit` as a `Set Char` and add some operations. We get the number 8 for free, and we can use it to invert other digits.

``` {.haskell #data-types-day-8}
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
```

Made a superfluous parser for the characters 'a' through 'g'.

``` {.haskell #parser-day-8}
word :: Parser Text
word = lexeme $ takeWhile1P (Just "letter a-g") (\c -> c >= 'a' && c <= 'g')

charSet :: Parser Digit
charSet = Digit . Set.fromList . Text.unpack <$> word

lineP :: Parser Line
lineP = Line <$> some charSet <* lexeme (char '|') <*> some charSet

readInput :: (HasLogFunc env) => RIO env [Line]
readInput = readInputParsing "data/day08.txt" (sepEndBy1 lineP eol)
```

Part A is very simple.

``` {.haskell #solution-day-8}
solutionA :: [Line] -> Int
solutionA = length . filter ((`elem` [2, 3, 4, 7]) . numberOfSegments)
                   . concatMap outputValues
```

Part B is not simple. To find the correct mapping we have to play around with deducing digits from the digits we already know. I used a **lazy** `Map Int (Maybe Digit)` to represent the partially decoded map. In the end I call `Map.mapMaybe` which is strict, because it needs to do pattern matching. In this lazy approach we need to make sure that all entries to the map are there, but the values are not evaluated until needed. We have a `match` function that checks if a digit matches a certain number.

``` {.haskell #solution-day-8}
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
              <<digit-decode-cases>>
              | otherwise            = False
              where l = numberOfSegments digit
```

The easy cases were already pointed to in part A:

``` {.haskell #digit-decode-cases}
| i == 1             = l == 2
| i == 4             = l == 4
| i == 7             = l == 3
| i == 8             = l == 7
| i `elem` [0, 6, 9] = l == 6 && 
    <<digit-6-segments>>
| i `elem` [2, 3, 5] = l == 5 &&
    <<digit-5-segments>>
```

In the case of five segments, i.e. numbers 2, 3 and 5, we can make the following deductions:

* digit 1 is a subset of 3 but not of 2 and 5
* digit 2 contains the segment that is not in 6
* digit 5 does not contain the segment that is not in 6

``` {.haskell #digit-5-segments}
fromMaybe False ( do
  one <- getDigit 1
  six <- getDigit 6
  return $ i == 3 && one <<< digit
        || i == 2 && invert six <<< digit
                  && not (one <<< digit)
        || i == 5 && not (invert six <<< digit)
                  && not (one <<< digit) )
```

In the case of six segments, i.e. numbers 0, 6 and 9, we can make the following deductions:

* the inverse of digit 0 (center segment) is in 4 and 1 is a subset of 0
* the inverse of digit 6 is in 1
* the digit 4 is a subset of digit 9

``` {.haskell #digit-6-segments}
fromMaybe False ( do
  one  <- getDigit 1
  four <- getDigit 4
  return $ i == 0 && invert digit <<< four
                  && one <<< digit
        || i == 6 && invert digit <<< one
        || i == 9 && four <<< digit )
```

Importantly, these deduction rules do not contain loops.

``` {.haskell #solution-day-8}
decodeLine :: Line -> Int
decodeLine Line{..} = fromDecimal $ mapMaybe (d !?) outputValues
    where d = decode signalPattern
          fromDecimal = foldl (\a b -> a * 10 + b) 0

solutionB :: [Line] -> Int
solutionB = sum . map decodeLine
```
