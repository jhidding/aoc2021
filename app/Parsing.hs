-- ~\~ language=Haskell filename=app/Parsing.hs
-- ~\~ begin <<lit/boilerplate.md|app/Parsing.hs>>[0]
module Parsing
    ( Parser, hspace, string, char, readInputParsing, lexeme
    , integer, eol, sepEndBy1, sepBy1, failOnException )
where

import RIO
import qualified RIO.Set as Set
import qualified RIO.Text as Text

import Text.Megaparsec
    ( ParseErrorBundle, Parsec, parse, errorBundlePretty, sepEndBy1
    , sepBy1, fancyFailure, ErrorFancy(..) )
import Text.Megaparsec.Char (hspace, string, char, eol)
import qualified Text.Megaparsec.Char.Lexer as L

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
integer = lexeme L.decimal
-- ~\~ end
