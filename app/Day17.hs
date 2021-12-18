-- ~\~ language=Haskell filename=app/Day17.hs
-- ~\~ begin <<lit/day17.md|app/Day17.hs>>[0]
module Day17 where

import RIO
import RIO.List (iterate)
import Print
import Parsing (Parser, readInputParsing, string, integer, lexeme, char)
import Linear.V2 (V2(..))

-- ~\~ begin <<lit/day17.md|data-types-day17>>[0]
data Area = Area
    { minX :: Int
    , maxX :: Int
    , minY :: Int
    , maxY :: Int
    } deriving (Show)

data PhaseSpace = PhaseSpace
    { position :: V2 Int
    , velocity :: V2 Int
    } deriving (Show)
-- ~\~ end
-- ~\~ begin <<lit/day17.md|parser-day17>>[0]
areaP :: Parser Area
areaP = Area <$ string "x=" <*> integer <* rangeSep <*> integer
             <* listSep
             <* string "y=" <*> integer <* rangeSep <*> integer
    where rangeSep = lexeme (string "..")
          listSep = lexeme (char ',')

readInput :: (HasLogFunc env) => RIO env Area
readInput = readInputParsing "data/day17.txt" (string "target area: " *> areaP)
-- ~\~ end
-- ~\~ begin <<lit/day17.md|solution-day17>>[0]
step :: PhaseSpace -> PhaseSpace
step (PhaseSpace position velocity@(V2 vx vy)) = PhaseSpace
    { position = position + velocity
    , velocity = V2 (vx - signum vx) (vy - 1)
    }
-- ~\~ end
-- ~\~ begin <<lit/day17.md|solution-day17>>[1]
hit :: Area -> PhaseSpace -> Bool
hit Area{..} (PhaseSpace (V2 x y) _) = minX <= x && x <= maxX
                                    && minY <= y && y <= maxY

miss :: Area -> PhaseSpace -> Bool
miss Area{..} (PhaseSpace (V2 x y) (V2 _ dy)) 
    = y < minY && dy < 0 || x > maxX
-- ~\~ end
-- ~\~ begin <<lit/day17.md|solution-day17>>[2]
velocityBounds :: Area -> (V2 Int, V2 Int)
velocityBounds Area{..} = (V2 minvx minvy, V2 maxvx maxvy)
    where minvy = minY
          maxvy = (-minY) - 1
          minvx = floor (sqrt (fromIntegral $ minX * 2))
          maxvx = maxX

data Outcome = Hit | Miss deriving (Eq)

iterateUntil :: (a -> a) -> (a -> Bool) -> a -> a
iterateUntil f p init
    | p init    = init
    | otherwise = iterateUntil f p (f init)

outcome :: Area -> V2 Int -> Outcome
outcome a v = if hit a last then Hit else Miss
    where last = iterateUntil step (\x -> hit a x || miss a x) 
                              (PhaseSpace (V2 0 0) v)
-- ~\~ end
-- ~\~ begin <<lit/day17.md|solution-day17>>[3]
solutionA :: Area -> Int
solutionA Area{..} = maxv * (maxv + 1) `div` 2
    where maxv = (- minY) - 1

solutionB :: Area -> Int
solutionB a = length [ V2 vx vy
                     | vx <- [minvx .. maxvx]
                     , vy <- [minvy .. maxvy]
                     , outcome a (V2 vx vy) == Hit ]
    where (V2 minvx minvy, V2 maxvx maxvy) = velocityBounds a
-- ~\~ end
-- ~\~ begin <<lit/day17.md|solution-day17>>[4]
iterateUntil' :: (a -> a) -> (a -> Bool) -> a -> [a]
iterateUntil' f p init
    | p init = [init]
    | otherwise = init : iterateUntil' f p (f init)

showData :: IO ()
showData = do
    area <- runSimpleApp readInput
    let (V2 minvx minvy, V2 maxvx maxvy) = velocityBounds area
        startVels = [ V2 vx vy
                           | vx <- [minvx .. maxvx]
                           , vy <- [minvy .. maxvy] ]
    mapM_ (printStats area) startVels
    printLn "\n"
    printLn $ "# " <> tshow (length startVels)
    mapM_ (printShot area) startVels
    where printShot area v = do
            let o = if outcome area v == Hit then 1 else 0
            let path = iterateUntil' step (\x -> hit area x || miss area x) 
                                (PhaseSpace (V2 0 0) v)
            when (length path > 10) (mapM_ (printPs o) path >> printLn "\n")
          printPs o (PhaseSpace (V2 x y) (V2 vx vy)) = printLn 
            $ tshow x <> " " <> tshow y <> " " <> tshow vx <> " "
            <> tshow vy <> " " <> tshow o
          printStats area v@(V2 vx vy) = do
            let o = if outcome area v == Hit then 1 else 0
            let l = if o == 1
                    then length $ iterateUntil' step (\x -> hit area x || miss area x) 
                                (PhaseSpace (V2 0 0) v)
                    else -1
            when (o == 1) $
                printLn $ tshow vx <> " " <> tshow vy <> " " <> tshow o <> " " <> tshow l
-- ~\~ end
-- ~\~ begin <<lit/day17.md|solution-day17>>[5]
invertArea :: Area -> Int -> Area
invertArea Area{..} t = Area minX' maxX' minY' maxY'
    where invertDelta x = floor (sqrt (fromIntegral $ 2 * x))
          invertQuadratic :: Int -> Float
          invertQuadratic x = fromIntegral x / fromIntegral t 
                            + (fromIntegral t - 1) / 2
          minX' = max (invertDelta minX) (ceiling $ invertQuadratic minX)
          maxX' = floor (invertQuadratic maxX)
          minY' = ceiling (invertQuadratic minY)
          maxY' = floor (invertQuadratic maxY)

printArea :: Area -> IO ()
printArea Area{..} = do
    printLn $ tshow minX <> " " <> tshow minY
    printLn $ tshow maxX <> " " <> tshow minY
    printLn $ tshow maxX <> " " <> tshow maxY
    printLn $ tshow minX <> " " <> tshow maxY
    printLn $ tshow minX <> " " <> tshow minY

showData2 :: IO ()
showData2 = do
    area <- runSimpleApp readInput
    mapM_ (\t -> printArea (invertArea area t) >> printLn "\n")
          [0 .. 2*(negate $ minY area)]
-- ~\~ end
-- ~\~ begin <<lit/boilerplate.md|run-solutions>>[0]
runA :: (HasLogFunc env) => RIO env ()
runA = readInput >>= logInfo . display . tshow . solutionA 

runB :: (HasLogFunc env) => RIO env ()
runB = readInput >>= logInfo . display . tshow . solutionB
-- ~\~ end
-- ~\~ end
