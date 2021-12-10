# Day 10: Syntax Scoring
Yay! Parsing! We can do this really well :)

``` {.haskell file=app/Day10.hs}
module Day10 where

import RIO hiding (lines)
import RIO.List (headMaybe)
import qualified RIO.Set as Set
import qualified RIO.Text as Text
import RIO.ByteString (readFile)
import RIO.Text (lenientDecode, decodeUtf8With, lines)
import Parsing (Parser, char, eol)
import Text.Megaparsec (parse, ParseErrorBundle(..), ErrorItem(..), ParseError(..))

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

illegalChar :: ParseErrorBundle Text Void -> Maybe Char
illegalChar e = case bundleErrors e of
                  (TrivialError _ (Just (Tokens (c :| _))) _) :| _ -> Just c
                  _                                                -> Nothing

scoreA :: Char -> Int
scoreA ')' = 3
scoreA ']' = 57
scoreA '}' = 1197
scoreA '>' = 25137
scoreA _   = 0

expectedChar :: ParseErrorBundle Text Void -> Maybe Char
expectedChar e = case bundleErrors e of
                   (TrivialError _ (Just EndOfInput) exp) :| _ -> getExpected exp
                   _                                           -> Nothing
    where getExpected :: Set (ErrorItem Char) -> Maybe Char
          getExpected s = headMaybe (Set.toList s) >>= getToken
          getToken (Tokens (c :| _)) = Just c
          getToken _                 = Nothing

scoreB :: Int -> Char -> Int
scoreB s ')' = s * 5 + 1
scoreB s ']' = s * 5 + 2
scoreB s '}' = s * 5 + 3
scoreB s '>' = s * 5 + 4
scoreB _ _   = 0

autocomplete :: Text -> Maybe Text
autocomplete orig = go ""
    where go suffix = either (complete suffix) (const $ Just suffix) (parseLine $ orig <> suffix)
          complete suffix err = do
              c <- expectedChar err
              go (suffix <> Text.singleton c)
--             (go . (suffix <>)) . Text.singleton =<< expectedChar err

readInput :: (MonadIO m) => m [Text]
readInput = lines . decodeUtf8With lenientDecode <$> readFile "data/day10.txt"

solutionA :: [Text] -> Int
solutionA = sum . map scoreA . mapMaybe illegalChar . lefts . map parseLine

solutionB :: [Text] -> [Text]
solutionB = mapMaybe autocomplete

runA :: (HasLogFunc env) => RIO env ()
runA = readInput >>= logInfo . display . tshow . solutionA 

runB :: (HasLogFunc env) => RIO env ()
runB = readInput >>= logInfo . display . tshow . solutionB
```

