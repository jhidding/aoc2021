# Day 23: Amphipod

``` {.haskell file=app/Day23.hs}
module Day23 where

import RIO
import qualified RIO.Map as Map
import qualified RIO.Text as Text
import RIO.State (State(..), evalState, get, modify)
import Text.Megaparsec (ParsecT(..), ParseErrorBundle, errorBundlePretty, runParserT, sepEndBy1)
import Text.Megaparsec.Char (char, eol)

import Linear.V2 (V2(..))

data Amphipod
    = Amber | Bronze | Copper | Desert
    deriving (Show, Eq, Ord)

data Tile
    = Wall
    | Pod Amphipod
    | Empty
    deriving (Show, Eq, Ord)

type Input = Map (V2 Int) Tile

type Parser a = ParsecT Void Text (State (V2 Int)) a

tileP :: Parser Tile
tileP =  (char '#' $> Wall)
     <|> (char 'A' $> Pod Amber)
     <|> (char 'B' $> Pod Bronze)
     <|> (char 'C' $> Pod Copper)
     <|> (char 'D' $> Pod Desert)
     <|> (char '.' $> Empty)

newlineP :: Parser ()
newlineP = eol >> modify (\(V2 x y) -> V2 0 (y+1))

cellP :: Parser (Maybe (V2 Int, Tile))
cellP = do
    tile <- (Just <$> tileP) <|> (Nothing <$ char ' ')
    loc <- get
    newlineP <|> modify (+ V2 1 0)
    return $ (loc,) <$> tile

inputP :: Parser Input
inputP = Map.fromList . catMaybes <$> some cellP

instance Display (ParseErrorBundle Text Void) where
    textDisplay = Text.pack . errorBundlePretty

readInput :: (HasLogFunc env) => RIO env Input
readInput = do
    txt <- readFileUtf8 "data/day23.txt"
    either (\e -> do { logError $ display e; exitFailure })
           return $ evalState (runParserT inputP "data/day23.txt" txt)
                              (V2 0 0)


solutionA = id
solutionB = const 0

<<run-solutions>>
```
