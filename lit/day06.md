# Day 6: Lanternfish
We need to simulate the number of lanternfish, each with a timer, spawning new lanternfish etc. Since we have an exponential growth process, to simulate this naively would be stupid, which is kind of the point of the exercise. We only have nine possible states for each fish, so instead we can tally **how many** lanternfish exist in each state. It turns out however, that programming it the stupid way first, turns this innocent looking exercise into a nice lesson on **Constraint Kinds**.

``` {.haskell file=app/Day06.hs}
module Day06 where

import RIO
import RIO.List (foldl)
import RIO.List.Partial (last)
import Parsing ( Parser, readInputParsing, sepEndBy1
               , lexeme, char, integer)
import RIO.Map (Map, (!?))
import qualified RIO.Map as Map
<<imports-day-6>>

<<parser-day-6>>
<<solution-day-6>>
<<run-solutions>>
```

As always, we first parse the input:

``` {.haskell #parser-day-6}
csvInts :: Parser [Int]
csvInts = sepEndBy1 integer (lexeme (char ','))

readInput :: (HasLogFunc env) => RIO env [Int]
readInput = readInputParsing "data/day06.txt" csvInts
```

The point of the exercise is that we can have a naive solution, which I implement here just for fun:

```haskell
rules :: Int -> [Int]
rules clock
    | clock == 0 = [8, 6]
    | otherwise  = [clock - 1]

step :: [Int] -> [Int]
step = (>>= rules)
```

We then iterate the `step` any number of times and get the length of the result:

``` {.haskell #solution-day-6}
iterate :: Int -> (a -> a) -> a -> [a]
iterate n f x
    | n == 0    = [x]
    | otherwise = x : iterate (n - 1) f (f x)

solutionA :: [Int] -> Int
solutionA = length . last . iterate 80 step
```

The problem is that this solution doesn't scale very well. To solve this more efficiently, we should keep track of how many fish are present in each state, then we can solve this problem in constant memory.

For tallying the amount of lanternfish in each state, I like to use a `Map Int Int`. 

``` {.haskell #tally}
newtype Tally a = Tally { tallyMap :: Map a Integer }
    deriving (Show)
```

Now we can implement `Semigroup` and `Monoid`:

``` {.haskell #tally}
instance (Ord a) => Semigroup (Tally a) where
    Tally a <> Tally b = Tally $ Map.unionWith (+) a b

instance (Ord a) => Monoid (Tally a) where
    mempty = Tally mempty
```

Now we could do something like,

```haskell
multiply :: (Ord a) => [a] -> Int -> Tally a
multiply items n = foldMap (\k -> Tally $ Map.singleton k n) items

concatMap :: (Ord a) => (a -> [a]) -> Tally a -> Tally a
concatMap f (Tally a) = Map.foldMapWithKey (multiply . f) a

step :: Tally Int -> Tally Int
step = concatMap rules
```

However, things could be even pretier if we could define something like `Applicative` on `Tally`.

## Associated Constraint Types
What if we could implement the naive version of this problem in such a way that we can easily scale it up later? We could say:

```haskell
rules :: (Applicative f, Semigroup (f Int)) => Int -> f Int
rules clock
    | clock == 0 = pure 8 <> pure 6
    | otherwise  = pure (clock - 1)
```

But this comes with another problem: our intended container `Tally` can never be a `Functor` or `Applicative`, since it only works on sortable `Ord` types. This kind of problem can only be solved if we are allowed associated constraint types with our class implementation. For this to work you need to enable the `TypeFamilies` and `ConstraintKinds` language extensions enabled.

We have to reimplement the `Functor > Applicative > Monad` stack.

``` {.haskell #tally}
class CFunctor f where
    type ElemCt f a :: Constraint
    cmap :: (ElemCt f a, ElemCt f b) => (a -> b) -> f a -> f b

class CFunctor f => CApplicative f where
    cpure :: (ElemCt f a) => a -> f a
    cliftA2 :: (ElemCt f a, ElemCt f b, ElemCt f c)
            => (a -> b -> c) -> f a -> f b -> f c
```

It is already impossible to implement the constraint version of `<*>` from the type signature. The default implementation of `cliftA2 id` assumes `ElemCt f (b -> c)` which we can never guarantee. There is no problem however defining `CMonad`.

``` {.haskell #tally}
class CApplicative f => CMonad f where
    cbind :: (ElemCt f a, ElemCt f b) => (a -> f b) -> f a -> f b
```

With these type classes in place, we can rewrite the solution to todays problem once again:

``` {.haskell #solution-day-6}
rules :: (CApplicative f, ElemCt f Int, Semigroup (f Int)) => Int -> f Int
rules fish
    | fish == 0 = cpure 8 <> cpure 6
    | otherwise = cpure (fish - 1)

step :: (CMonad f, ElemCt f Int, Semigroup (f Int)) => f Int -> f Int
step = cbind rules
```

### Implementation for List
There is the little annoyance that we need to be able to signal an `Empty` constraint:

``` {.haskell #tally}
class EmptyCt a
instance EmptyCt a
```

We now need to implement `CMonad` on lists and we should have our first naive implementation back in working order.

``` {.haskell #tally}
instance CFunctor [] where
    type ElemCt [] a = EmptyCt a
    cmap = fmap

instance CApplicative [] where
    cpure = pure
    cliftA2 = liftA2

instance CMonad [] where
    cbind = (=<<)
```

This even means we could have `do` notation on constraint monads without loss of generality!

### Implementation for `Tally`

``` {.haskell #tally}
instance CFunctor Tally where
    type ElemCt Tally a = Ord a
    cmap f (Tally a) = Map.foldMapWithKey (\k v -> Tally (Map.singleton (f k) v)) a

multiply :: Tally a -> Integer -> Tally a
multiply (Tally a) n = Tally (Map.map (* n) a)

instance CApplicative Tally where
    cpure a = Tally $ Map.singleton a 1
    cliftA2 f (Tally a) b = Map.foldMapWithKey
            (\k v -> multiply (cmap (f k) b) v) a

instance CMonad Tally where
    cbind f (Tally a) = Map.foldMapWithKey (multiply . f) a
```

Notice that the implementation of `cliftA2` is as if the elements were all stored in a list. This is motivated by the linear property that `(f <*> a) <> (f <*> b) == f <*> (a <> b)`. We don't need `cliftA2` in our problem, but I included it here for completeness.

``` {.haskell #tally}
size :: Tally a -> Integer
size (Tally a) = sum $ Map.elems a

singleton :: Ord a => a -> Tally a
singleton = cpure

fromList :: Ord a => [a] -> Tally a
fromList = foldMap cpure

distinct :: Ord a => Tally a -> [a]
distinct (Tally a) = Map.keys a
```

``` {.haskell file=app/Tally.hs}
module Tally where

import RIO
import qualified RIO.Map as Map
import Data.Constraint (Constraint)

<<tally>>
```

``` {.haskell #imports-day-6}
import Tally (Tally, CFunctor(..), CApplicative(..), CMonad(..), ElemCt)
import qualified Tally
```

``` {.haskell #solution-day-6}
solutionB :: [Int] -> Integer
solutionB = Tally.size . last . iterate 256 step . Tally.fromList
```

