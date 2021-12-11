-- ~\~ language=Haskell filename=app/Day11.hs
-- ~\~ begin <<lit/day11.md|app/Day11.hs>>[0]
module Day11 where

import RIO
import RIO.ByteString (putStr)
import qualified RIO.Text as Text
import RIO.State (evalStateT, evalState, execState, MonadState, modify, get, gets)
import Data.Massiv.Array (Ix2(..))
import qualified Data.Massiv.Array as A
import Parsing (Parser, sepEndBy1, failOnException, eol, digit, readInputParsing)

-- ~\~ begin <<lit/day11.md|parser-day-11>>[0]
-- ~\~ begin <<lit/day09.md|digit-array-parser>>[0]
type Array2' r a = A.Array r A.Ix2 a
type Array2 a = Array2' A.U a

digitArray :: Parser (Array2 Int)
digitArray = sepEndBy1 (some digit) eol >>= toArray2
    where toArray2 = failOnException . A.fromListsM A.Seq
-- ~\~ end

readInput :: (HasLogFunc env) => RIO env (Array2 Int)
readInput = readInputParsing "data/day11.txt" digitArray
-- ~\~ end
-- ~\~ begin <<lit/day11.md|solution-day-11>>[0]
step :: (MonadState (Array2 Int) m) => m Int
step = clock >> flash >> reset
-- ~\~ end
-- ~\~ begin <<lit/day11.md|solution-day-11>>[1]
clock :: (MonadState (Array2 Int) m) => m ()
clock = modify (A.compute . A.map (+ 1))
-- ~\~ end
-- ~\~ begin <<lit/day11.md|solution-day-11>>[2]
home :: A.Ix2
home = 0 :. 0

neighbours :: [A.Ix2]
neighbours = [ -1 :. -1, 0 :. -1, 1 :. -1
             , -1 :.  0,          1 :.  0
             , -1 :.  1, 0 :.  1, 1 :.  1 ]

count :: (a -> Bool) -> [a] -> Int
count f = sum . map (\x -> if f x then 1 else 0)

countArray :: (A.Unbox a) => (a -> Bool) -> Array2 a -> Int
countArray f = A.sum . A.map (\x -> if f x then 1 else 0)

flashed :: Int -> Bool
flashed c = c > 9 && c < 1000

flashStencil :: A.Stencil Ix2 Int Int
flashStencil = A.makeStencil (A.Sz (3 :. 3)) (1 :. 1) go
    where go get = if flashed (get home) then 1000
                   else get home + count (flashed . get) neighbours

flash :: MonadState (Array2 Int) m => m ()
flash = do
    n <- gets $ countArray flashed
    if n == 0 then return ()
    else modify go >> flash
    where go :: Array2 Int -> Array2 Int
          go = A.compute . A.mapStencil (A.Fill 0) flashStencil
-- ~\~ end
-- ~\~ begin <<lit/day11.md|solution-day-11>>[3]
reset :: MonadState (Array2 Int) m => m Int
reset = do
    n <- gets $ countArray (>= 1000)
    modify $ A.compute . A.map (\x -> if x >= 1000 then 0 else x)
    return n
-- ~\~ end
-- ~\~ begin <<lit/day11.md|solution-day-11>>[4]
repeatM :: (Applicative m) => Int -> m a -> m [a]
repeatM n a = loop n
    where loop n
            | n <= 0    = pure []
            | otherwise = liftA2 (:) a (loop (n - 1))

solutionA :: Array2 Int -> Int
solutionA = sum . evalState (repeatM 100 step)

countRepeatUntilM :: (Monad m) => m Bool -> m Int
countRepeatUntilM action = go 1
    where go n = do
            stop <- action
            if stop then return n else go (n + 1)

solutionB :: Array2 Int -> Int
solutionB = evalState $ countRepeatUntilM ((== 100) <$> step)
-- ~\~ end

-- ~\~ begin <<lit/boilerplate.md|run-solutions>>[0]
runA :: (HasLogFunc env) => RIO env ()
runA = readInput >>= logInfo . display . tshow . solutionA 

runB :: (HasLogFunc env) => RIO env ()
runB = readInput >>= logInfo . display . tshow . solutionB
-- ~\~ end
-- ~\~ end
