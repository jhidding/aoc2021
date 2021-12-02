---
date: 2021-11-30
summary: Prep
title: Boilerplate
weight: 10
---

# Appendix: Boiler plate

<div class="named-code-block">

«read-integer-list»

``` haskell
readInput :: MonadIO m => m [Int]
readInput = do
    text <- Text.lines <$> readFileUtf8 "data/day01.txt"
    return $ mapMaybe (readMaybe . Text.unpack) text
```

</div>

<div class="named-code-block">

«run-solutions»

``` haskell
runA :: (HasLogFunc env) => RIO env ()
runA = do 
    result <- solutionA <$> readInput
    logInfo $ display result

runB :: (HasLogFunc env) => RIO env ()
runB = do 
    result <- solutionB <$> readInput
    logInfo $ display result
```

</div>

# Appendix: Parsing

<div class="named-code-block">

file:app/Parsing.hs

``` haskell
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

</div>
