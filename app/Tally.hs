-- ~\~ language=Haskell filename=app/Tally.hs
-- ~\~ begin <<lit/day06.md|app/Tally.hs>>[0]
module Tally where

import RIO
import qualified RIO.Map as Map
import Data.Constraint (Constraint)

-- ~\~ begin <<lit/day06.md|tally>>[0]
newtype Tally a = Tally { tallyMap :: Map a Integer }
    deriving (Show)
-- ~\~ end
-- ~\~ begin <<lit/day06.md|tally>>[1]
instance (Ord a) => Semigroup (Tally a) where
    Tally a <> Tally b = Tally $ Map.unionWith (+) a b

instance (Ord a) => Monoid (Tally a) where
    mempty = Tally mempty
-- ~\~ end
-- ~\~ begin <<lit/day06.md|tally>>[2]
class CFunctor f where
    type ElemCt f a :: Constraint
    cmap :: (ElemCt f a, ElemCt f b) => (a -> b) -> f a -> f b

class CFunctor f => CApplicative f where
    cpure :: (ElemCt f a) => a -> f a
    cliftA2 :: (ElemCt f a, ElemCt f b, ElemCt f c)
            => (a -> b -> c) -> f a -> f b -> f c
-- ~\~ end
-- ~\~ begin <<lit/day06.md|tally>>[3]
class CApplicative f => CMonad f where
    cbind :: (ElemCt f a, ElemCt f b) => (a -> f b) -> f a -> f b
-- ~\~ end
-- ~\~ begin <<lit/day06.md|tally>>[4]
class EmptyCt a
instance EmptyCt a
-- ~\~ end
-- ~\~ begin <<lit/day06.md|tally>>[5]
instance CFunctor [] where
    type ElemCt [] a = EmptyCt a
    cmap = fmap

instance CApplicative [] where
    cpure = pure
    cliftA2 = liftA2

instance CMonad [] where
    cbind = (=<<)
-- ~\~ end
-- ~\~ begin <<lit/day06.md|tally>>[6]
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
-- ~\~ end
-- ~\~ begin <<lit/day06.md|tally>>[7]
size :: Tally a -> Integer
size (Tally a) = sum $ Map.elems a

singleton :: Ord a => a -> Tally a
singleton = cpure

fromList :: Ord a => [a] -> Tally a
fromList = foldMap cpure

distinct :: Ord a => Tally a -> [a]
distinct (Tally a) = Map.keys a
-- ~\~ end
-- ~\~ begin <<lit/day21.md|tally>>[0]
instance (Integral n) => Num (Tally n) where
    (+) = cliftA2 (+)
    (-) = cliftA2 (-)
    (*) = cliftA2 (*)
    negate = cmap negate
    abs = cmap abs
    fromInteger = singleton . fromInteger
    signum = cmap signum
-- ~\~ end
-- ~\~ end
