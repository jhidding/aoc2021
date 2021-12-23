# Day 23: Amphipod

``` {.haskell file=app/Day23.hs}
module Day23 where

import RIO
import RIO.List (find)
import RIO.List.Partial (tail)
import qualified RIO.Map as Map
import qualified RIO.Text as Text
import RIO.State (State(..), evalState, get, modify)
import Data.Tuple (swap)

import Text.Megaparsec (ParsecT(..), ParseErrorBundle, errorBundlePretty, runParserT, sepEndBy1)
import Text.Megaparsec.Char (char, eol)

import Linear.V2 (V2(..))
import Dijkstra (minDist)

data AmphipodType
    = Amber | Bronze | Copper | Desert
    deriving (Show, Eq, Ord)

data Tile
    = Wall
    | Amphipod AmphipodType
    | Empty
    deriving (Show, Eq, Ord)

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

readInput :: (HasLogFunc env) => RIO env World
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

destination :: Map AmphipodType [V2 Int]
destination = Map.fromList
    [(Amber,  [V2 3 5, V2 3 4, V2 3 3, V2 3 2])
    ,(Bronze, [V2 5 5, V2 5 4, V2 5 3, V2 5 2])
    ,(Copper, [V2 7 5, V2 7 4, V2 7 3, V2 7 2])
    ,(Desert, [V2 9 5, V2 9 4, V2 9 3, V2 9 2])]

inside :: [V2 Int]
inside = join $ Map.elems destination

outside :: [V2 Int]
outside = [V2 1 1, V2 2 1, V2 4 1, V2 6 1, V2 8 1, V2 10 1, V2 11 1]

pathOut :: (V2 Int, V2 Int) -> [V2 Int]
pathOut (V2 x y, V2 u v) = up <> side
    where up   = [V2 x y' | y' <- [y, y-1 .. v+1]]
          side = [V2 x' v | x' <- [x, x + signum (u - x) .. u]]

pathIn :: (V2 Int, V2 Int) -> [V2 Int]
pathIn = reverse . pathOut . swap

pathFree :: World -> [V2 Int] -> Bool
pathFree world = all ((== Just Empty) . (world Map.!?)) . tail

roomAvailable :: World -> AmphipodType -> Maybe (V2 Int)
roomAvailable world amph = do
    cells <- destination Map.!? amph
    let occupants = filter isAmphipod $ mapMaybe (world Map.!?) cells
    if all (== Amphipod amph) occupants
    then find ((== Just Empty) . (world Map.!?)) cells
    else Nothing

legalMoveIn :: World -> [(V2 Int, V2 Int)]
legalMoveIn world = filter (pathFree world . pathIn)  candidates
    where candidates = [(src, tgt) | src <- outside
                                   , maybe False isAmphipod (world Map.!? src)
                                   , tgt <- maybeToList (roomAvailable world 
                                        =<< amphipodType =<< world Map.!? src)]

legalMoveOut :: World -> [(V2 Int, V2 Int)]
legalMoveOut world = filter (pathFree world . pathOut) candidates
    where candidates = [(src, tgt) | src <- inside
                                   , maybe False isAmphipod (world Map.!? src)
                                   , tgt <- outside
                                   , world Map.!? tgt == Just Empty]

cost :: World -> (V2 Int, V2 Int) -> Maybe Int
cost world (src, tgt) = do
    amph <- amphipodType =<< world Map.!? src
    gpm <- energyPerMove Map.!? amph
    let dist = sum (abs (tgt - src))
    return $ dist * gpm

legalMoves :: World -> [((V2 Int, V2 Int), Int)]
legalMoves w = mapMaybe addCost $ legalMoveIn w <> legalMoveOut w
    where addCost m = (m,) <$> cost w m

applyMove :: World -> (V2 Int, V2 Int) -> World
applyMove world (src, tgt) = Map.update (\_ -> world Map.!? src) tgt
                           $ Map.insert src Empty world

solutionA :: World -> Maybe Int
solutionA world = minDist legalMoves applyMove world goal
    where goal = Map.fromList (foldMap (\(a, cs) -> (,Amphipod a) <$> cs)
                                       (Map.toList destination)) <> world

solutionB = const 0

<<run-solutions>>
```
