module Main where

import Data.List
import Control.Applicative
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L


sum' xs = foldl' (\x y -> x + y) 0 xs

sumMaybes :: (Num a) => Maybe a -> Maybe a -> Maybe a
sumMaybes x y = pure (\a b -> a + b) <*> x <*> y

newtype Parser a = P (L8.ByteString -> Maybe(a,L8.ByteString))

runParser :: Parser a -> L8.ByteString -> Maybe (a,L8.ByteString)
runParser (P f) = f

main :: IO ()
main = do print "joopajoo"
