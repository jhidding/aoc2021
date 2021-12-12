-- ~\~ language=Haskell filename=app/Day12.hs
-- ~\~ begin <<lit/day12.md|app/Day12.hs>>[0]
{-# LANGUAGE TypeApplications #-}
module Day12 where

import RIO hiding (try)
import RIO.Map ((!?))
import RIO.Set ((\\))
import qualified RIO.Map as Map
import qualified RIO.Set as Set
import qualified RIO.Text as Text
import Parsing (Parser, string, char, eol, sepEndBy1, readInputParsing)
import RIO.Char (isLower, isUpper)
import Text.Megaparsec (takeWhile1P, try)

-- ~\~ begin <<lit/day12.md|parser-day-12>>[0]
data Cave = Start | End | Big Text | Small Text
    deriving (Eq, Ord)

instance Show Cave where
    show Start = "start"
    show End = "end"
    show (Big c) = Text.unpack c
    show (Small c) = Text.unpack c

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
-- ~\~ end
-- ~\~ begin <<lit/day12.md|solution-day-12>>[0]
type CaveMap = Map Cave [Cave]

routing :: [Link] -> CaveMap
routing = Map.unionsWith (<>) . map linkToMap
    where linkToMap (Link a b) = Map.fromList [(a, [b]), (b, [a])]
-- ~\~ end
-- ~\~ begin <<lit/day12.md|solution-day-12>>[1]
class CaveSet s where
    visit   :: Cave -> s -> s
    allowed :: Cave -> s -> Bool
-- ~\~ end
-- ~\~ begin <<lit/day12.md|solution-day-12>>[2]
instance CaveSet (Set Cave) where
    visit (Big _) s = s
    visit c       s = Set.insert c s
    allowed         = Set.notMember
-- ~\~ end
-- ~\~ begin <<lit/day12.md|solution-day-12>>[3]
findRoutesTo :: (CaveSet s) => CaveMap -> s -> Cave -> Cave -> [[Cave]]
findRoutesTo caveMap visited end start
    | start == end = [[end]]
    | otherwise    = map (start :) $ concatMap recur nextRooms
    where visited'   = visit start visited
          recur      = findRoutesTo caveMap visited' end
          nextRooms  = filter (`allowed` visited')
                     $ fromMaybe [] (caveMap !? start)

findRoutesA :: CaveMap -> [[Cave]]
findRoutesA caveMap = findRoutesTo caveMap (Set.empty @Cave) End Start

solutionA :: [Link] -> Int
solutionA = length . findRoutesA . routing
-- ~\~ end
-- ~\~ begin <<lit/day12.md|solution-day-12>>[4]
data AugmentedSet a = AugmentedSet (Set a) (Maybe a)
    deriving (Show)

instance CaveSet (AugmentedSet Cave) where
    visit (Big _) s = s
    visit (Small c) (AugmentedSet s Nothing)
      | Small c `Set.member` s = AugmentedSet s (Just (Small c))
      | otherwise              = AugmentedSet (Set.insert (Small c) s) Nothing
    visit c (AugmentedSet s m)
      | c `Set.member` s       = error $ "Cave " <> show c <> " was already passed"
      | otherwise              = AugmentedSet (Set.insert c s) m

    allowed (Small c) (AugmentedSet s Nothing) = True
    allowed i (AugmentedSet s _) = i `Set.notMember` s

findRoutesB :: CaveMap -> [[Cave]]
findRoutesB caveMap = findRoutesTo caveMap emptySet End Start
    where emptySet = AugmentedSet @Cave Set.empty Nothing

solutionB :: [Link] -> Int
solutionB = length . findRoutesB . routing
-- ~\~ end
-- ~\~ begin <<lit/boilerplate.md|run-solutions>>[0]
runA :: (HasLogFunc env) => RIO env ()
runA = readInput >>= logInfo . display . tshow . solutionA 

runB :: (HasLogFunc env) => RIO env ()
runB = readInput >>= logInfo . display . tshow . solutionB
-- ~\~ end
-- ~\~ end
