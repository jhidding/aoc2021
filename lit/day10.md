# Day 10: Syntax Scoring
Yay! Parsing! We can do this really well :)

``` {.haskell file=app/Day10.hs}
module Day10 where

import RIO hiding (lines)
import RIO.List.Partial ((!!))
import RIO.List (sort, headMaybe, foldl)
import qualified RIO.Set as Set
import qualified RIO.Text as Text
import RIO.ByteString (readFile)
import RIO.Text (lenientDecode, decodeUtf8With, lines)
import Parsing (Parser, char, eol)
import Text.Megaparsec ( parse, ParseErrorBundle(..), ErrorItem(..)
                       , ParseError(..))

<<parsing-day-10>>
<<solution-day-10>>
<<run-solutions>>
```

Parsing these sequences is what we have `Megaparsec` for.

``` {.haskell #parsing-day-10}
data Bracket = Round | Square | Curly | Angle
    deriving (Show, Eq, Ord, Enum)

data Chunk = Chunk Bracket [Chunk]
    deriving (Show)

openingBracket :: Parser Bracket
openingBracket =  char '(' $> Round
              <|> char '{' $> Curly
              <|> char '[' $> Square
              <|> char '<' $> Angle

closingBracket :: Bracket -> Parser ()
closingBracket b = (case b of
                     Round  -> char ')'
                     Square -> char ']'
                     Curly  -> char '}'
                     Angle  -> char '>') $> ()

chunkP :: Parser Chunk
chunkP = do
    opening <- openingBracket
    content <- many chunkP
    closingBracket opening
    return $ Chunk opening content

parseLine :: Text -> Either (ParseErrorBundle Text Void) Chunk
parseLine = parse chunkP ""

readInput :: (MonadIO m) => m [Text]
readInput = lines . decodeUtf8With lenientDecode 
         <$> readFile "data/day10.txt"
```

For part A we need to look at the parser error that we get and extract the unexpected character. We can pattern match to get at the character and assume if it doesn't match, we have unexpected end-of-input.

``` {.haskell #solution-day-10}
illegalChar :: ParseErrorBundle Text Void -> Maybe Char
illegalChar e = case bundleErrors e of
    (TrivialError _ (Just (Tokens (c :| _))) _) :| _ -> Just c
    _                                                -> Nothing
```

Completing the score,

``` {.haskell #solution-day-10}
scoreA :: Char -> Int
scoreA ')' = 3
scoreA ']' = 57
scoreA '}' = 1197
scoreA '>' = 25137
scoreA _   = 0

solutionA :: [Text] -> Int
solutionA = sum . map scoreA . mapMaybe illegalChar
          . lefts . map parseLine
```

In part B we look at the characters we expected when encountering end-of-input. We need to take care here: opening brackets are always expected, so we filter on closing brackets.

``` {.haskell #solution-day-10}
expectedChar :: ParseErrorBundle Text Void -> Maybe Char
expectedChar e = case bundleErrors e of
    (TrivialError _ (Just EndOfInput) exp) :| _ -> getExpected exp
    _                                           -> Nothing
    where getExpected :: Set (ErrorItem Char) -> Maybe Char
          getExpected s = headMaybe $ concatMap getToken
                                    $ Set.toList s
          getToken (Tokens (t :| ts)) = filter closingChar (t : ts)
          getToken _                  = []
          closingChar = (`elem` [')', ']', '}', '>'])
```

To autocomplete, I keep re-parsing the string, adding characters at the end, until the parsing succeeds. In principle, this could be done nicer from the parser, by creating a sort of stack trace. However, that would polute the code for actually parsing the correct structure.

``` {.haskell #solution-day-10}
autocomplete :: Text -> Maybe Text
autocomplete orig = go ""
    where go suffix = either (complete suffix)
                             (const $ Just suffix)
                             (parseLine $ orig <> suffix)
          complete suffix err = do
              c <- expectedChar err
              go (suffix <> Text.singleton c)
```

For computing the score, we encounter our old friend the `median` function again.

``` {.haskell #solution-day-10}
scoreB :: Text -> Int
scoreB = foldl f 0 . Text.unpack 
    where f i c = i * 5 + s c
          s ')' = 1
          s ']' = 2
          s '}' = 3
          s '>' = 4
          s _   = 0

median :: [Int] -> Int
median x = sort x !! (length x `div` 2)

solutionB :: [Text] -> Int
solutionB = median . map scoreB . mapMaybe autocomplete
```

