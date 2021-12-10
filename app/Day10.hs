-- ~\~ language=Haskell filename=app/Day10.hs
-- ~\~ begin <<lit/day10.md|app/Day10.hs>>[0]
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

-- ~\~ begin <<lit/day10.md|parsing-day-10>>[0]
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

-- ~\~ begin <<lit/day10.md|read-lines>>[0]
readLines :: (MonadIO m) => m [Text]
readLines = Text.lines . Text.decodeUtf8With Text.lenientDecode 
         <$> readFile "data/day10.txt"
-- ~\~ end

readInput :: (MonadIO m) => m [Text]
readInput = readLines
-- ~\~ end
-- ~\~ begin <<lit/day10.md|solution-day-10>>[0]
illegalChar :: ParseErrorBundle Text Void -> Maybe Char
illegalChar e = case bundleErrors e of
    (TrivialError _ (Just (Tokens (c :| _))) _) :| _ -> Just c
    _                                                -> Nothing
-- ~\~ end
-- ~\~ begin <<lit/day10.md|solution-day-10>>[1]
scoreA :: Char -> Int
scoreA ')' = 3
scoreA ']' = 57
scoreA '}' = 1197
scoreA '>' = 25137
scoreA _   = 0

solutionA :: [Text] -> Int
solutionA = sum . map scoreA . mapMaybe illegalChar
          . lefts . map parseLine
-- ~\~ end
-- ~\~ begin <<lit/day10.md|solution-day-10>>[2]
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
-- ~\~ end
-- ~\~ begin <<lit/day10.md|solution-day-10>>[3]
autocomplete :: Text -> Maybe Text
autocomplete orig = go ""
    where go suffix = either (complete suffix)
                             (const $ Just suffix)
                             (parseLine $ orig <> suffix)
          complete suffix err = do
              c <- expectedChar err
              go (suffix <> Text.singleton c)
-- ~\~ end
-- ~\~ begin <<lit/day10.md|solution-day-10>>[4]
-- ~\~ begin <<lit/day10.md|median>>[0]
median :: [Int] -> Int
median x = sort x !! (length x `div` 2)
-- ~\~ end

scoreB :: Text -> Int
scoreB = foldl f 0 . Text.unpack 
    where f i c = i * 5 + s c
          s ')' = 1
          s ']' = 2
          s '}' = 3
          s '>' = 4
          s _   = 0

solutionB :: [Text] -> Int
solutionB = median . map scoreB . mapMaybe autocomplete
-- ~\~ end
-- ~\~ begin <<lit/boilerplate.md|run-solutions>>[0]
runA :: (HasLogFunc env) => RIO env ()
runA = readInput >>= logInfo . display . tshow . solutionA 

runB :: (HasLogFunc env) => RIO env ()
runB = readInput >>= logInfo . display . tshow . solutionB
-- ~\~ end
-- ~\~ end
