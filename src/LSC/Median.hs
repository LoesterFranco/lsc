
module LSC.Median where

import Control.Monad.ST
import Data.Function (on)
import Data.Vector
import Data.Vector.Mutable (read)
import Data.Vector.Algorithms.Intro

import Prelude hiding (length, read)



exactMedian :: Ord a => Vector a -> a
exactMedian = exactMedianBy (compare `on` id)

{-# SPECIALIZE exactMedian :: Vector Int -> Int #-}


exactMedianBy :: (a -> a -> Ordering) -> Vector a -> a
exactMedianBy ordering v = runST $ do
    u <- thaw v
    sortBy ordering u
    read u $ div (length v) 2

