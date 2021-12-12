# Day 12: Passage Pathing
Graphs! The fun times we live in :)

``` {.haskell file=app/Day12.hs}
{-# LANGUAGE TypeApplications #-}
module Day12 where

import RIO hiding (try)
import RIO.Map ((!?))
import RIO.Set ((\\))
import qualified RIO.Map as Map
import qualified RIO.Set as Set
import Parsing (Parser, string, char, eol, sepEndBy1, readInputParsing)
import RIO.Char (isLower, isUpper)
import Text.Megaparsec (takeWhile1P, try)

<<parser-day-12>>
<<solution-day-12>>
<<run-solutions>>
```

``` {.haskell #parser-day-12}
data Cave = Start | End | Big Text | Small Text
    deriving (Show, Eq, Ord)

data Link = Link Cave Cave
    deriving (Show, Eq, Ord)

caveP :: Parser Cave
caveP =  (Start <$  try (string "start"))
     <|> (End   <$  try (string "end" ))
     <|> (Big   <$> takeWhile1P (Just "A-Z") isUpper)
     <|> (Small <$> takeWhile1P (Just "a-z") isLower)

linkP :: Parser Link
linkP = Link <$> caveP <* char '-' <*> caveP

readInput :: (HasLogFunc env) => RIO env [Link]
readInput = readInputParsing "data/day12.txt" (sepEndBy1 linkP eol)
```

``` {.haskell #solution-day-12}
type CaveMap = Map Cave [Cave]

data AugmentedSet a = AugmentedSet (Set a) (Maybe a)

class CaveSet s where
    insert  :: Cave -> s -> s
    allowed :: Cave -> s -> Bool

instance CaveSet (Set Cave) where
    insert  = Set.insert
    allowed = Set.notMember

routing :: [Link] -> CaveMap
routing = Map.unionsWith (<>) . map linkToMap
    where linkToMap (Link a b) = Map.fromList [(a, [b]), (b, [a])]

findRoutesTo :: (CaveSet s) => CaveMap -> s -> Cave -> Cave -> [[Cave]]
findRoutesTo caveMap visited end start
    | start == end = [[end]]
    | otherwise    = map (start :) $ concatMap (findRoutesTo caveMap (visit start) end) nextRooms
    where visit (Big x) = visited
          visit x       = insert x visited
          nextRooms     = filter (`allowed` visited) $ fromMaybe [] (caveMap !? start)

findRoutesA :: CaveMap -> [[Cave]]
findRoutesA caveMap = findRoutesTo caveMap (Set.empty @Cave) End Start

solutionA :: [Link] -> Int
solutionA = length . findRoutesA . routing

instance CaveSet (AugmentedSet Cave) where
    insert (Small c) (AugmentedSet s Nothing)
      | Small c `Set.member` s = AugmentedSet s (Just (Small c))
      | otherwise              = AugmentedSet (Set.insert (Small c) s) Nothing
    insert c (AugmentedSet s m) = AugmentedSet (Set.insert c s) m

    allowed (Small c) (AugmentedSet s Nothing) = True
    allowed i (AugmentedSet s _) = i `Set.notMember` s

findRoutesB :: CaveMap -> [[Cave]]
findRoutesB caveMap = findRoutesTo caveMap (AugmentedSet @Cave Set.empty Nothing) End Start

solutionB :: [Link] -> Int
solutionB = length . findRoutesB . routing
```

