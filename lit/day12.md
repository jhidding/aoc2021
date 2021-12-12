# Day 12: Passage Pathing
Graphs! The fun times we live in :). We're getting a map of caves. This is my input:

``` {.make #day-12-plot-map target=fig/day12-map.svg}
$(target): data/day12.txt build/day12-to-dot.awk
    awk -f build/day12-to-dot.awk $< | dot -Tsvg > $@
```

``` {.awk .hide file=build/day12-to-dot.awk}
BEGIN {
    print "graph {"
    print "    rankdir=LR"
    print "    bgcolor=\"#00000000\""
    print "    node [colorscheme=oranges3 color=2 fontcolor=1]"
    print "    edge [colorscheme=oranges3 color=2]"
}

match($0, /([a-zA-Z]+)-([a-zA-Z]+)/, m) {
    print "    ", m[1], "--", m[2]
}

END {
    print "}"
}
```

``` {.haskell file=app/Day12.hs}
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

<<parser-day-12>>
<<solution-day-12>>
<<run-solutions>>
```

I distinguish the `Start`, `End` caves and `Big` and `Small` during parsing.

``` {.haskell #parser-day-12}
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
```

From the list of `Link` we can extract a `CaveMap`.

``` {.haskell #solution-day-12}
type CaveMap = Map Cave [Cave]

routing :: [Link] -> CaveMap
routing = Map.unionsWith (<>) . map linkToMap
    where linkToMap (Link a b) = Map.fromList [(a, [b]), (b, [a])]
```

With part B in mind, we need to have an abstract `Cave` container with two methods, `visit` and `allowed`.

``` {.haskell #solution-day-12}
class CaveSet s where
    visit   :: Cave -> s -> s
    allowed :: Cave -> s -> Bool
```

In part A, we can use a `Set Cave` to keep track of all the caves we visited:

* A big cave is always allowed, so we do not enter it into the set.
* Any other cave is only allowed once.

``` {.haskell #solution-day-12}
instance CaveSet (Set Cave) where
    visit (Big _) s = s
    visit c       s = Set.insert c s
    allowed         = Set.notMember
```

I find all possible routes recursively, using `concatMap`.

``` {.haskell #solution-day-12}
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
```

Now for part B. We need a container that allows one item to appear twice. I call this `AugmentedSet`. All I need to do is implement `CaveSet` on this new container and we're done!

``` {.haskell #solution-day-12}
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
```

