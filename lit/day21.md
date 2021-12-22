# Day 21: Dirac Dice
I reused the `Tally` structure that I made on day 6, and extended it such that `Tally Int` supports numeric operations. This way I computed the answer using distributions of integers, and distributions of game states.

What I started with, was writing a monadic type class for playing the game. Considering that the game is independent for both players, I tried to solve this by simulation each player separately, but I got stuck in the bookkeeping. Then decided that keeping tally of numbers of game states was easier.

``` {.haskell file=app/Day21.hs}
module Day21 where

import RIO
import RIO.List (find, cycle, iterate, scanl')
import RIO.State (MonadState, State, get, gets, put, modify, execState)

import Parsing (readInputParsing, string, integer, eol)
import Lens.Micro.Platform ((&), (<%~), use, (%=), (.=), (<%=), (<<%=))
import qualified Tally
import Tally (Tally)

<<parser-day21>>
<<abstract-day21>>
<<game-state-day21>>
<<solution-day21>>
<<run-solutions>>
```

Obligatory parser of the day:

``` {.haskell #parser-day21}
type Input = (Int, Int)
inputP = (,) <$ string "Player 1 starting position: " <*> integer <* eol
             <* string "Player 2 starting position: " <*> integer <* eol

readInput :: (HasLogFunc env) => RIO env Input
readInput = readInputParsing "data/day21.txt" inputP
```

## Abstract implementation
Specifying game rules is often cleanest using a monadic type class. Here I have an associated type that gives the equivalent of a scalar (so `Int` for part A, and `Tally Int` for part B).

``` {.haskell #abstract-day21}
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
```

## Game data
To encode the game state, I have two data types `PlayerData` and `GameData`. Then some lenses to ease into manipulating these types. To be honest, I don't like lenses. There are too many operators and combinators making the code start to look like APL or J (yes, that's a bad thing).

``` {.haskell #game-state-day21}
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
```

Now we can define some helper functions to do moving and scoring.

``` {.haskell #game-state-day21}
select Player1 = player1
select Player2 = player2

gmove :: Player -> Int -> GameData -> GameData
gmove player step state = 
    state & select player . pos
          %~ (\x -> (x + step - 1) `mod` 10 + 1)

gscore :: Player -> GameData -> GameData
gscore player state =
    state & select player . score
          %~ (+ state ^. select player . pos)
```

## Part A
Now to solve part A, we run the game in a `State` monad.

``` {.haskell #solution-day21}
newtype GameA a = GameA { gameStateA :: State GameData a }
    deriving (Functor, Applicative, Monad, MonadState GameData)

instance Game GameA where
    type Dist GameA = Int
    roll = do x <- use die100
              die100 %= (+ 1)
              return $ x `mod` 100 + 1

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
```

## Part B
For part B we can reuse everything we already have, replacing the normal integers with a `Tally Int`, and keeping game state in a `Tally GameState`.

### Calculating with distributions
Implementing `Num` on `Tally n`. It is interesting to note that multiplication in this space is not the same as repeated addition. This makes sense though. Doing

```haskell
>>> let a = fromList [1, 2, 3]
>>> 3 * a
[(3, 1), (6, 1), (9, 1)]
>>> a + a + a
[(3, 1), (4, 3), (5, 6), (6, 7), (7, 6), (8, 3), (9, 1)]
```

Given the overengineered solution of day 6, implementing `Num` is straight forward.

``` {.haskell #tally}
instance (Integral n) => Num (Tally n) where
    (+) = cliftA2 (+)
    (-) = cliftA2 (-)
    (*) = cliftA2 (*)
    negate = cmap negate
    abs = cmap abs
    fromInteger = singleton . fromInteger
    signum = cmap signum
```

### Game state
The `GameState` now includes a state that tells if a player has alread won. The `gwin` function (monadic on `Either Player a`) determines if the game state should switch to one of the players winning.

``` {.haskell #solution-day21}
type GameState = Either Player GameData

gwin :: Player -> GameData -> GameState
gwin player gameData =
    if gameData ^. select player . score >= 21
    then Left player else Right gameData
```

With it comes a new `tmove` action that is monadic over `Tally a`. It's almost a shame that I have to enter explicitely that further dice throws can be ignored after a player has won. Maybe there is a way to use the monadic action over `Either Player a` to stop the game when its over, but I haven't found it.

``` {.haskell #solution-day21}
tmove :: Player -> Tally Int -> GameState -> Tally GameState
tmove _      _  g@(Left _)   = Tally.singleton g
tmove player step (Right gameData) =
    Tally.cmap (\i -> gwin player $ gscore player
                    $ gmove player i gameData) step
```

### Running the game
Considering the increase of complexity, it is amazing how little work we needed to do to solve part B now. (Of course, this is a cleaned up version, and it took me ages to figure out this solution.)

``` {.haskell #solution-day21}
newtype GameB a = GameB { gameStateB :: State (Tally GameState) a }
    deriving (Functor, Applicative, Monad, MonadState (Tally GameState))

instance Game GameB where
    type Dist GameB = Tally Int
    roll = return $ Tally.fromList [1, 2, 3]
    move p i = modify (Tally.cbind (tmove p i))
    stop = gets (all isLeft . Tally.distinct)

runGameB :: Input -> Tally GameState
runGameB (p1, p2) = execState (gameStateB play)
    $ Tally.singleton $ Right
    $ GameData 0 (PlayerData p1 0) (PlayerData p2 0)

solutionB = runGameB
```
