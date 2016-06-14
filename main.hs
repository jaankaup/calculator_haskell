module Main where

import Data.List
import Control.Applicative

sum' xs = foldl' (\x y -> x + y) 0 xs

sumMaybes :: (Num a) => Maybe a -> Maybe a -> Maybe a
sumMaybes x y = pure (\a b -> a + b) <*> x <*> y
