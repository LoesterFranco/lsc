
module LSC.Median where

import Control.Monad.ST
import Data.Vector
import Data.Vector.Mutable (read)
import Data.Vector.Algorithms.Intro

import Prelude hiding (length, read)



exactMedian :: Ord a => Vector a -> a
exactMedian = exactMedianBy compare
{-# INLINE exactMedian #-}
{-# SPECIALIZE exactMedian :: Vector Int -> Int #-}


exactMedianBy :: (a -> a -> Ordering) -> Vector a -> a
exactMedianBy ordering v = runST $ do
    u <- thaw v
    let k = length v `div` 2
    partialSortBy ordering u (succ k)
    read u k

