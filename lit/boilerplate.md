# Appendix: Boiler plate

``` {.haskell #run-solutions}
runA :: (HasLogFunc env) => RIO env ()
runA = readInput >>= logInfo . display . solutionA 

runB :: (HasLogFunc env) => RIO env ()
runB = readInput >>= logInfo . display . solutionB
```

# Appendix: Parsing

``` {.haskell file=app/Parsing.hs}
module Parsing
    ( Parser, hspace, string, char, readInputParsing, lexeme, integer, eol
    , sepEndBy1, sepBy1 )
where

import RIO
import qualified RIO.Text as Text

import Text.Megaparsec
    ( ParseErrorBundle, Parsec, parse, errorBundlePretty, sepEndBy1, sepBy1 )
import Text.Megaparsec.Char (hspace, string, char, eol)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text
instance Display (ParseErrorBundle Text Void) where
    textDisplay = Text.pack . errorBundlePretty

readInputParsing :: (MonadReader env m, MonadIO m, HasLogFunc env) => FilePath -> Parser a -> m a
readInputParsing file p = do
    x <- parse p file <$> readFileUtf8 file
    either (\e -> do { logError $ display e; exitFailure })
           return x

lexeme :: Parser a -> Parser a
lexeme = L.lexeme hspace

integer :: Parser Int
integer = lexeme L.decimal
```
