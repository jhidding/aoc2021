-- ~\~ language=Haskell filename=app/Day05.hs
-- ~\~ begin <<lit/day05.md|app/Day05.hs>>[0]
module Day05 where

import RIO hiding (try)
import RIO.ByteString (putStr)
import qualified RIO.Text as Text
import RIO.List.Partial (foldl1)
import RIO.List (partition, headMaybe, lastMaybe)
import Parsing ( Parser, sepEndBy1, sepBy1, char, hspace, eol
               , integer, lexeme, readInputParsing
               , string )
import qualified Data.Massiv.Array as A
import qualified Data.Massiv.Array.Mutable as MA

-- ~\~ begin <<lit/day05.md|data-types-day-5>>[0]
newtype Vec2 a = Vec2 (a, a)

instance Show a => Show (Vec2 a) where
    show (Vec2 (x, y)) = "(" <> show x <> " " <> show y <> ")"

instance Functor Vec2 where
    fmap f (Vec2 (x, y)) = Vec2 (f x, f y)

instance Applicative Vec2 where
    pure x = Vec2 (x, x)
    liftA2 f (Vec2 (ax, ay)) (Vec2 (bx, by)) = Vec2 (f ax bx, f ay by)

type Pos = Vec2 Int

pos :: Int -> Int -> Pos
pos x y = Vec2 (x, y)

type Line = (Pos, Pos)

makeLine :: Int -> Int -> Int -> Int -> Line
makeLine x1 y1 x2 y2 = (pos x1 y1, pos x2 y2)
-- ~\~ end
-- ~\~ begin <<lit/day05.md|data-types-day-5>>[1]
type Diagram = A.Array A.P A.Ix2 Int
type MutDiagram s = MA.MArray s A.P A.Ix2 Int
-- ~\~ end
-- ~\~ begin <<lit/day05.md|parser-day-5>>[0]
lineP :: Parser Line
lineP = makeLine <$> integer <* lexeme (char ',') <*> integer
                 <*  lexeme (string "->")
                 <*> integer <* lexeme (char ',') <*> integer

readInput :: (HasLogFunc env) => RIO env [Line]
readInput = readInputParsing "data/day05.txt" (sepEndBy1 lineP eol)
-- ~\~ end
-- ~\~ begin <<lit/day05.md|solution-day-5>>[0]
lineMinMax :: Line -> (Pos, Pos)
lineMinMax (a, b) = (min <$> a <*> b, max <$> a <*> b)

totalMinMax :: [Line] -> (Pos, Pos)
totalMinMax ls = foldl1 minMax $ lineMinMax <$> ls
    where minMax (a, b) (c, d) = (min <$> a <*> c, max <$> b <*> d)
-- ~\~ end
-- ~\~ begin <<lit/day05.md|solution-day-5>>[1]
plotCoords :: MutDiagram s -> [A.Ix2] -> ST s ()
plotCoords d = mapM_ (MA.modify_ d (return . (+ 1)))
-- ~\~ end
-- ~\~ begin <<lit/day05.md|solution-day-5>>[2]
range :: Int -> Int -> [Int]
range a b
    | a > b = reverse [b .. a]
    | otherwise = [a .. b]

lineCoords :: Line -> [A.Ix2]
lineCoords l
    -- ~\~ begin <<lit/day05.md|day-5-line-cases>>[0]
    | horizontal l = horizontalCoords l
    -- ~\~ end
    -- ~\~ begin <<lit/day05.md|day-5-line-cases>>[1]
    | vertical l = verticalCoords l
    -- ~\~ end
    -- ~\~ begin <<lit/day05.md|day-5-line-cases>>[2]
    | diagonal l = diagonalCoords l
    -- ~\~ end
    | otherwise = error $ "Illegal line: " <> show l
-- ~\~ end
-- ~\~ begin <<lit/day05.md|solution-day-5>>[3]
horizontal :: Line -> Bool
horizontal (Vec2 (_, ay), Vec2 (_, by)) = ay == by

horizontalCoords :: Line -> [A.Ix2]
horizontalCoords (Vec2 (ax, y), Vec2 (bx, _))
    = A.toIx2 . (,y) <$> range ax bx
-- ~\~ end
-- ~\~ begin <<lit/day05.md|solution-day-5>>[4]
vertical :: Line -> Bool
vertical (Vec2 (ax, _), Vec2 (bx, _)) = ax == bx

verticalCoords :: Line -> [A.Ix2]
verticalCoords (Vec2 (x, ay), Vec2 (_, by))
    = A.toIx2 . (x,) <$> range ay by
-- ~\~ end
-- ~\~ begin <<lit/day05.md|solution-day-5>>[5]
plotLines :: [Line] -> Diagram
plotLines l = runST $ do
    arr <- MA.newMArray (A.Sz2 1000 1000) 0
    mapM_ (plotCoords arr . lineCoords) l
    MA.freezeS arr

solutionA :: [Line] -> Int
solutionA = length . filter (> 1) . A.toList 
          . plotLines . filter (not . diagonal)
-- ~\~ end
-- ~\~ begin <<lit/day05.md|solution-day-5>>[6]
diagonal :: Line -> Bool
diagonal (Vec2 (ax, ay), Vec2 (bx, by))
    = abs (ax - bx) == abs (ay - by)

diagonalCoords :: Line -> [A.Ix2]
diagonalCoords (Vec2 (ax, ay), Vec2 (bx, by))
    = A.toIx2 <$> zip (range ax bx) (range ay by)

solutionB :: [Line] -> Int
solutionB = length . filter (> 1) . A.toList . plotLines
-- ~\~ end
-- ~\~ begin <<lit/day05.md|extra-day-5>>[0]
showData :: IO ()
showData = runSimpleApp $ do
    inp <- readInput
    let result = plotLines inp
    mapM_ (\(Vec2 (ax, ay), Vec2 (bx, by))
           -> print $ tshow ax <> " " <> tshow ay <> " " <> tshow bx <> " " <> tshow by <> "\n")
          inp
    print "\n\n"
    print $ Text.intercalate "\n" $ map (Text.intercalate " " . map tshow) (A.toLists2 result)
    return ()
    where print = putStr . Text.encodeUtf8
-- ~\~ end
-- ~\~ begin <<lit/boilerplate.md|run-solutions>>[0]
runA :: (HasLogFunc env) => RIO env ()
runA = readInput >>= logInfo . display . tshow . solutionA

runB :: (HasLogFunc env) => RIO env ()
runB = readInput >>= logInfo . display . tshow . solutionB
-- ~\~ end
-- ~\~ end
