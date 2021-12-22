-- ~\~ language=Haskell filename=app/Day21.hs
-- ~\~ begin <<lit/day21.md|app/Day21.hs>>[0]
module Day21 where

import RIO
import RIO.List (find, cycle, iterate, scanl')
import RIO.State (MonadState, State, get, gets, put, modify, execState)

import Parsing (readInputParsing, string, integer, eol)
import Lens.Micro.Platform ((&), (<%~), use, (%=), (.=), (<%=), (<<%=))
import qualified Tally
import Tally (Tally)

type Input = (Int, Int)
inputP = (,) <$ string "Player 1 starting position: " <*> integer <* eol
             <* string "Player 2 starting position: " <*> integer <* eol

readInput :: (HasLogFunc env) => RIO env Input
readInput = readInputParsing "data/day21.txt" inputP

data Player = Player1 | Player2
    deriving (Show, Eq, Ord)

class Monad m => Game m where
    type Dist m :: *
    roll :: m (Dist m)
    move :: Player -> Dist m -> m ()
    stop :: m Bool

turn :: (Game m, Num (Dist m)) => Player -> m ()
turn p = do
    a <- roll
    b <- roll
    c <- roll
    move p (a + b + c)

runUntilM :: (Monad m) => (a -> m Bool) -> [a] -> m ()
runUntilM _ [] = return ()
runUntilM p (x:xs) = do
    q <- p x
    unless q (runUntilM p xs)

play :: (Game m, Num (Dist m)) => m ()
play = runUntilM (\x -> turn x >> stop) (cycle [Player1, Player2])

data PlayerData = PlayerData
    { _playerPos :: Int
    , _playerScore :: Int
    } deriving (Show, Eq, Ord)

pos :: Lens' PlayerData Int
pos = lens _playerPos (\p x -> p{_playerPos = x})
score :: Lens' PlayerData Int
score = lens _playerScore (\p x -> p{_playerScore = x})

data GameData = GameData
    { _die100 :: Int
    , _player1 :: PlayerData
    , _player2 :: PlayerData
    } deriving (Show, Eq, Ord)

die100 :: Lens' GameData Int
die100 = lens _die100 (\g d -> g{_die100 = d})
player1 :: Lens' GameData PlayerData
player1 = lens _player1 (\g p -> g{_player1 = p})
player2 :: Lens' GameData PlayerData
player2 = lens _player2 (\g p -> g{_player2 = p})

newtype GameA a = GameA { gameStateA :: State GameData a }
    deriving (Functor, Applicative, Monad, MonadState GameData)

select Player1 = player1
select Player2 = player2

instance Game GameA where
    type Dist GameA = Int
    roll = do { x <- die100 <<%= (+ 1);
                return $ x `mod` 100 + 1 }
    move p i = modify (gscore p . gmove p i)
    stop = do 
        p1 <- use $ player1 . score
        p2 <- use $ player2 . score
        return (p1 >= 1000 || p2 >= 1000)

runGameA :: Input -> GameData
runGameA (p1, p2) = execState (gameStateA play)
    $ GameData 0 (PlayerData p1 0) (PlayerData p2 0)

solutionA = output . runGameA
    where output g = min (g ^. player1 . score)
                         (g ^. player2 . score)
                   * (g ^. die100)

type GameState = Either Player GameData

gmove :: Player -> Int -> GameData -> GameData
gmove p i gd = gd & select p . pos %~ (\x -> (x + i - 1) `mod` 10 + 1)

gscore :: Player -> GameData -> GameData
gscore p gd = gd & select p . score %~ (+ s)
    where s = gd ^. select p . pos

gwin p gd = if gd ^. select p . score >= 21
               then Left p else Right gd

newtype GameB a = GameB { gameStateB :: State (Tally GameState) a }
    deriving (Functor, Applicative, Monad, MonadState (Tally GameState))

solve :: Player -> Tally Int -> GameState -> Tally GameState
solve p _ g@(Left _) = Tally.singleton g
solve p i (Right gd) = Tally.cmap (\i -> gwin p $ gscore p $ gmove p i gd) i

instance Game GameB where
    type Dist GameB = Tally Int
    roll = return $ Tally.fromList [1, 2, 3]
    move p i = modify (Tally.cbind (solve p i))
    stop = gets (all isLeft . Tally.distinct)

runGameB :: Input -> Tally GameState
runGameB (p1, p2) = execState (gameStateB play)
    $ Tally.singleton $ Right
    $ GameData 0 (PlayerData p1 0) (PlayerData p2 0)

solutionB = runGameB
-- ~\~ begin <<lit/boilerplate.md|run-solutions>>[0]
runA :: (HasLogFunc env) => RIO env ()
runA = readInput >>= logInfo . display . tshow . solutionA

runB :: (HasLogFunc env) => RIO env ()
runB = readInput >>= logInfo . display . tshow . solutionB
-- ~\~ end
-- ~\~ end
