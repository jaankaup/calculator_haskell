{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

{- Yksinkertainen ohjelma aritmeettisten lausekkeiden laskentaan. 
   Copyright: Janne Kauppinen.
   Version 0.00002 alpha. 
   None rights reserved.
 -}

module Parser where

import Data.Char (isDigit)
import Data.List (stripPrefix)
import Data.Monoid
import Control.Applicative
 
type Writer a = (a,String)


(>=>) :: (a -> Writer b) -> (b -> Writer c) -> a -> Writer c
m1 >=> m2 = \x -> let (r1,s) = m1 x
                      (r2,s') = m2 r1 
                  in (r2, s <> s')

return :: a -> Writer a
return x = (x,mempty)

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

cout (C x) = show x
cout ((:+) x y) = show x ++ " + " ++ show y
cout ((:-) x y) = show x ++ " - " ++ show y
cout ((:*) x y) = show x ++ " * " ++ show y

unFx :: Fix f -> (f (Fix f))
unFx (Fx x) = x

cata :: Functor f => (f a -> a) -> Fix f -> a
cata algebra = algebra . fmap (cata algebra) . unFx

--cata2 algebra = algebra . fmap (\x -> (cata algebra, show . unFx))

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
   fmap f (P fx) = P (fmap (\(x,s) -> (f x, s)) . fx)

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

expr2 :: Parser (Fix (Expr Double))
expr2 = ex 
     where ex = add <> sub <> p1 
           p1 = mult <> p2 
           p2 = const <> par ex   
           const = fmap (Fx . C) double 
           add = fmap Fx $ pure (:+) <*> (p1 <* string "+") <*> ex
           sub = fmap Fx $ pure (:-) <*> (p1 <* string "-") <*> ex
           mult = fmap Fx $ pure (:*) <*> (p2 <* string "*") <*> p1
             

multD :: Parser DoubleExpr 
multD = exp
          where exp = fmap Fx $ pure (:*) <*> cons <*> cons 
                cons = pure (Fx . C) <*> (a <* b) 
                a   = (string " " *> a) <> double 
                b   = (string " " *> b) <> (string "*")

evaluate a s = case runParser a s of
                 Nothing -> Nothing
                 Just (v,"") -> Just $ cata evalAlgebra v
                 Just _      -> Nothing

runParser :: Parser a -> String -> Maybe (a,String)
runParser (P f) = f

execParser :: Parser a -> String -> Maybe a
execParser (P f) = fmap fst . f

takeP :: (Char -> Bool) -> Parser String
takeP p = P $ \x -> case span p x of
                     ("",_) -> Nothing
                     r      -> Just r  

takeOneP :: (Char -> Bool) -> Parser String
takeOneP p = P $ \x -> case x of
                         ""     -> Nothing
                         (y:ys) -> if p y then Just ([y],ys) else Nothing    

takeOneD :: Parser Int
takeOneD = fmap read $ takeOneP isDigit 

int :: Parser Int
int = fmap read $ takeP isDigit

number :: Parser Int
number = int <> (fmap read $ string "-" +++ toString int) 

string :: String -> Parser String
string s = P $ \x -> case stripPrefix s x of
                       Nothing -> Nothing
                       Just e  -> Just (s,e)

par :: Parser a -> Parser a
par = between "(" ")"

between :: String -> String -> Parser a -> Parser a
between l r p = (string l) *> p <* (string r)

--fxParser :: Parser (Expr a b) -> Parser (Fix (Expr a))
--fxParser = fmap Fx

-- Kontekstiton kielioppi, joka tuottaa doublen:
-- S -> minus A | digit A | digit B | d
-- A -> digit A | digit B | d
-- B -> ee C    | digit D 
-- C -> plus F  | minus F | digit F | d
-- D -> digit E | d
-- E -> ee C
-- F -> digit F | d
-- plus  -> +  
-- minus -> -
-- ee    -> e
-- dot   -> .
-- digit -> d
--
-- Pitaisi nyt olla Chomskyn normaalimuodossa. Tosin Haskelissa on hieman oikaistu
-- yksittaisten digit:ien kanssa. Ne pitaisi ainakin teoriassa olla paatemerkkeja
-- eika valikemerkkeja ollessaan ainoana merkkina. Tosin kielioppi ei tarkista
-- sita, onko jasennetty merkkijono lukualueeltaan liian suuri tai pieni mahtumaan
-- doubleen. Ts. voi tulla Infinity tai -Infinity. 

double :: Parser Double
double = P $ \x -> let s = (minus +++ a) <> (digit +++ a) <> (digit +++ b) <> digit
                       a = (digit +++ a) <> (digit +++ b) <> digit
                       b = (ee +++ c) <> (dot +++ d) 
                       c = (plus +++ f) <> (minus +++ f) <> (digit +++ f) <> digit 
                       d = (digit +++ e) <> f--digit 
                       e = ee +++ c 
                       f = (digit +++ f) <> digit
                       plus   = string "+"
                       minus  = string "-"
                       ee     = string "e"
                       dot    = string "."
                       digit  = toString takeOneD
                   in case runParser s x of 
                        Nothing            -> Nothing
                        Just (result,rest) -> Just (read result, rest)

toString :: Show a => Parser a -> Parser String 
toString pa = fmap show pa

(+++) :: Parser String -> Parser String -> Parser String
pa +++ pb = pure (++) <*> pa <*> pb

main = do 
      putStrLn $ cata cout testExp 
