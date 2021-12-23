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

data AmphipodType
    = Amber | Bronze | Copper | Desert
    deriving (Show, Eq, Ord)

data Tile
    = Wall
    | Amphipod AmphipodType
    | Empty
    deriving (Show, Eq, Ord)

isEmpty :: Tile -> Bool
isEmpty Empty = True
isEmpty _     = False

isAmphipod :: Tile -> Bool
isAmphipod (Amphipod _) = True
isAmphipod _            = False

amphipodType :: Tile -> Maybe AmphipodType
amphipodType (Amphipod x) = Just x
amphipodType _            = Nothing

type World = Map (V2 Int) Tile

type Parser a = ParsecT Void Text (State (V2 Int)) a

tileP :: Parser Tile
tileP =  (char '#' $> Wall)
     <|> (char 'A' $> Amphipod Amber)
     <|> (char 'B' $> Amphipod Bronze)
     <|> (char 'C' $> Amphipod Copper)
     <|> (char 'D' $> Amphipod Desert)
     <|> (char '.' $> Empty)

newlineP :: Parser ()
newlineP = eol >> modify (\(V2 x y) -> V2 0 (y+1))

cellP :: Parser (Maybe (V2 Int, Tile))
cellP = do
    tile <- (Just <$> tileP) <|> (Nothing <$ char ' ')
    loc <- get
    newlineP <|> modify (+ V2 1 0)
    return $ (loc,) <$> tile

inputP :: Parser World
inputP = Map.fromList . catMaybes <$> some cellP

instance Display (ParseErrorBundle Text Void) where
    textDisplay = Text.pack . errorBundlePretty

readInput :: (HasLogFunc env) => RIO env Input
readInput = do
    txt <- readFileUtf8 "data/day23.txt"
    either (\e -> do { logError $ display e; exitFailure })
           return $ evalState (runParserT inputP "data/day23.txt" txt)
                              (V2 0 0)

energyPerMove :: Map AmphipodType Int
energyPerMove = Map.fromList
    [(Amber, 1), (Bronze, 10), (Copper, 100), (Desert, 1000)]

neighbours :: V2 Int -> [V2 Int]
neighbours p = (p +) <$> [V2 (-1) 0, V2 1 0, V2 0 (-1), V2 0 1]

destinations :: Map AmphipodType [V2 Int]
destinations = Map.fromList
    [(Amber,  [V2 3 2, V2 3 3])
    ,(Bronze, [V2 5 2, V2 5 3])
    ,(Copper, [V2 7 2, V2 7 3])
    ,(Desert, [V2 9 2, V2 9 3])]

keepMoving :: [V2 Int]
keepMoving = [V2 3 1, V2 5 1, V2 7 1, V2 9 1]

legalMoves :: World -> [(V2 Int, V2 Int, Int)]
legalMoves w =
    where candidates = Map.filterWithKey (\k v -> isAmphipod v && canMove k) w
          canMove p = any mapMaybe (w Map.!?) (neighbours p)

solutionA = id
solutionB = const 0

<<run-solutions>>
```
