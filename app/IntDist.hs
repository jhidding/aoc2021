module IntDist (IntDist(..), findMin, map, bind, singleton, fromList) where

import RIO hiding (map)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

newtype IntDist n = IntDist { toIntMap :: IntMap n }
    deriving (Show)

instance (Integral n) => Semigroup (IntDist n) where
    IntDist a <> IntDist b = IntDist (IntMap.unionWith (+) a b)

instance (Integral n) => Monoid (IntDist n) where
    mempty = IntDist IntMap.empty

singleton :: (Integral n) => Int -> IntDist n
singleton x = IntDist (IntMap.singleton x 1)

fromList :: (Integral n) => [Int] -> IntDist n
fromList = foldMap singleton

multiply :: (Integral n) => IntDist n -> n -> IntDist n
multiply (IntDist x) n = IntDist (IntMap.map (* n) x)

map :: (Int -> Int) -> IntDist n -> IntDist n
map f (IntDist x) = IntDist (IntMap.mapKeys f x)

findMin :: IntDist n -> Int
findMin (IntDist x) = fst $ IntMap.findMin x

bind :: (Integral n) => IntDist n -> (Int -> IntDist n) -> IntDist n
bind (IntDist x) f = IntMap.foldMapWithKey (multiply . f) x

applyOp :: (Integral n) => (Int -> Int -> Int) -> IntDist n
        -> IntDist n -> IntDist n
applyOp f a b = bind a (\i -> map (f i) b)

instance (Integral n) => Num (IntDist n) where
    (+) = applyOp (+)
    (-) = applyOp (-)
    (*) = applyOp (*)
    negate = map negate
    abs = map abs
    fromInteger = singleton . fromInteger
    signum = map signum

