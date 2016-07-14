{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

{- Yksinkertainen Haskelilla toteutettu tulkki. 
   Copyright: Janne Kauppinen.
   Version 0.00001 alpha. 
   None rights reserved.
 -}

module Parser where

import Data.Char (isDigit)
import Data.List (stripPrefix)

newtype Fix f = Fx (f (Fix f)) 

type Algebra f a = Functor f => f a -> a 

data Expr a b = C a
              | (:+) b b
              | (:*) b b
              | (:-) b b
              | (:/) b b
              | Par b
              deriving (Functor, Show)

type IntExpr = Fix (Expr Int)
type StringExpr = Fix (Expr String)
type DoubleExpr = Fix (Expr Double)

type IntAlgebra = Algebra (Expr Int) Int
type StringAlgebra = Algebra (Expr String) String

class Functor f => Eval f a | f -> a where
  evalAlgebra :: f a -> a

instance Num a => Eval (Expr a) a where
  evalAlgebra (C x) = x 
  evalAlgebra ((:+) x y) = x + y
  evalAlgebra ((:-) x y) = x - y
  evalAlgebra ((:*) x y) = x * y
--  evalAlgebra ((:/) x y) = x `div` y -- TODO: jako nollalla!

unFx :: Fix f -> (f (Fix f))
unFx (Fx x) = x

cata :: Functor f => (f a -> a) -> Fix f -> a
cata algebra = algebra . fmap (cata algebra) . unFx

alg :: IntAlgebra 
alg (C x) = x
alg ((:+) x y) = x + y
alg ((:*) x y) = x * y

algS :: StringAlgebra 
algS (C x) = x
algS ((:+) x y) = x ++ y
algS ((:*) x y) = concat [[a,b] | a <- x, b <- y]

testExp :: IntExpr
testExp = Fx $ (:+) (Fx $ (:*) (Fx $ C 5) (Fx $ C 3)) (Fx $ C 9)

testExpD :: DoubleExpr
testExpD = Fx $ (:+) (Fx $ (:*) (Fx $ C 5.1) (Fx $ C 3.1)) (Fx $ C 9.4)

testExpS :: StringExpr
testExpS = Fx $ (:+) (Fx $ C "kissa ") (Fx $ C "istuu!")

test = Fx $ (:*) (Fx $ C 5) (Fx $ C 3)


-- Jasennin-osio

newtype Parser a = P (String -> Maybe (a,String))

instance Functor Parser where
   fmap f (P x) = P (fmap (\(x,s) -> (f x, s)) . x)

instance Applicative Parser where
   pure x = P $ \s -> Just (x,s)
   fa <*> fb = P $ \s -> case runParser fa s of
                           Nothing      -> Nothing
                           Just (p,s')  -> case runParser fb s' of
                              Nothing -> Nothing
                              Just (x,s'') -> Just (p x, s'')

instance Monoid (Parser a) where
   mempty = P $ const Nothing 
   mappend a b = P $ \x -> case runParser a x of
                             Just a  -> Just a
                             Nothing -> runParser b x   


runParser :: Parser a -> String -> Maybe (a,String)
runParser (P f) = f

execParser :: Parser a -> String -> Maybe a
execParser (P f) = fmap fst . f

takeP :: (Char -> Bool) -> Parser String
takeP p = P $ \x -> case span p x of
                     ("",_) -> Nothing
                     r      -> Just r  

int :: Parser Int
int = fmap read (takeP isDigit)

string :: String -> Parser String
string s = P $ \x -> case stripPrefix s x of
                       Nothing -> Nothing
                       Just e  -> Just (s,e)     
                        
