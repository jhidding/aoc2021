# Appendix: Boiler plate

``` {.haskell #run-solutions}
runA :: (HasLogFunc env) => RIO env ()
runA = readInput >>= logInfo . display . tshow . solutionA 

runB :: (HasLogFunc env) => RIO env ()
runB = readInput >>= logInfo . display . tshow . solutionB
```

# Appendix: Parsing

``` {.haskell file=app/Parsing.hs}
module Parsing
    ( Parser, hspace, string, char, readInputParsing, lexeme
    , integer, eol, sepEndBy1, sepBy1, failOnException, digit
    , digitArray, dropUntilEol )
where

import RIO
import RIO.Char (ord, GeneralCategory(LineSeparator), generalCategory)
import qualified RIO.Set as Set
import qualified RIO.Text as Text

import Text.Megaparsec
    ( ParseErrorBundle, Parsec, parse, errorBundlePretty, sepEndBy1
    , sepBy1, fancyFailure, ErrorFancy(..), takeWhileP )
import Text.Megaparsec.Char (hspace, string, char, eol)
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Data.Massiv.Array as A

type Parser = Parsec Void Text

instance Display (ParseErrorBundle Text Void) where
    textDisplay = Text.pack . errorBundlePretty

failOnException :: (Exception e) => Either e a -> Parser a
failOnException = either convertError return
    where convertError = fancyFailure . Set.singleton
                       . ErrorFail . displayException

readInputParsing :: (MonadReader env m, MonadIO m, HasLogFunc env)
                 => FilePath -> Parser a -> m a
readInputParsing file p = do
    x <- parse p file <$> readFileUtf8 file
    either (\e -> do { logError $ display e; exitFailure })
           return x

lexeme :: Parser a -> Parser a
lexeme = L.lexeme hspace

integer :: Parser Int
integer = do
    sign_ <- maybe 1 (const (-1)) <$> optional (char '-')
    abs_  <- lexeme L.decimal
    return (sign_ * abs_)

digit :: Parser Int
digit = toValue <$> C.digitChar
    where toValue c = ord c - ord '0'

dropUntilEol :: Parser ()
dropUntilEol = void $ takeWhileP (Just "reading to eol")
                                 (/= '\n')
                    >> eol

<<digit-array-parser>>
```
