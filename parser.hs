{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

{- Yksinkertainen ohjelma aritmeettisten lausekkeiden laskentaan. 
   Kaytossa ovat numerot ja seuraavat merkit: ()+-*/. Numerot ovat liukulukuja, ja myos e-merkki on mukana.
   Esimerkkeja liukuluvuista, joita ohjelma tunnistaa: 13, -13.13, -13e-15, 13.1313e+12.
   Copyright: Janne Kauppinen.
   Version 0.00004 beta. 
   None rights reserved. Use at your own risk.
 -}

module Main where

import Data.Char (isDigit,isLetter)
import Data.List (stripPrefix)
import Data.Monoid
import Control.Applicative
import Control.Monad (unless)
import System.IO (hFlush, stdout)
import Prelude hiding(div)
 
-- Tyyppirakentaja tyyppien kiintopisteille.
newtype Fix f = Fx (f (Fix f)) 

-- "Saanto" sille, miten tyyppi a toimii, kun aletaan evaluoida.
type Algebra f a = Functor f => f a -> a 

data Expr a b = C a
              | b :+ b
              | b :- b 
              | b :* b
              | b :/ b
              | Par b
              deriving (Functor, Show)

-- Jasennyspuun tyyppi tassa ohjelmassa.
type DoubleExpr = Fix (Expr Double)

-- Jasennyspuun tyyppi, jos paatesymbolit ovatkin merkkijonoja. Ei kaytossa tassa ohjelmassa.
type StringExpr = Fix (Expr String)

-- Jasennyspuun tyyppi, jos kaytettaisiin Stringeja. Tosin taytyy maaritella miinus ja jako-operaatiot Stringille ensin. 
type StringAlgebra = Algebra (Expr String) String

-- Eval rajapinta.
class Functor f => Eval f a | f -> a where
  evalAlgebra :: f a -> a

-- Evaluaointia varten. Tassa ohjelmassa saanto double-tyyppisille arvoille. 
instance RealFrac a => Eval (Expr a) a where
  evalAlgebra (C x) = x 
  evalAlgebra ((:+) x y) = x + y
  evalAlgebra ((:-) x y) = x - y
  evalAlgebra ((:*) x y) = x * y
  evalAlgebra ((:/) x y) = x / y -- Jako nollalla! Antaa Infinity:n double tapauksessa. Ei haittaa tassa ohjelmassa.

-- Unfiksaus. 
unFx :: Fix f -> (f (Fix f))
unFx (Fx x) = x

-- Evaluointi-funktio. Jasennyspuu kaydaan rekursiivisesti lapi.
-- Argumenttinä saatua algebraa sovelletaan puun solmuihin.
cata :: Functor f => (f a -> a) -> Fix f -> a
cata algebra = algebra . fmap (cata algebra) . unFx

-- Olkoon tama viela mukana demonstraation vuoksi. Nyt jos halutaan evaluoida Stringeja, niin cata funktiolle annetaan algS-algebra, niin talloin 
-- tiedetaan miten hoidetaan Stringien aritmeettisia operaatioita. Tosin miinus ja jako-operaatioita ei ole viela keksitty :).
algS :: StringAlgebra 
algS (C x) = x
algS ((:+) x y) = x ++ y
algS ((:*) x y) = concat [[a,b] | a <- x, b <- y]

-- Testi DoubleExpr:n.
testExpD :: DoubleExpr
testExpD = Fx $ (:+) (Fx $ (:*) (Fx $ C 5.1) (Fx $ C 3.1)) (Fx $ C 9.4)

-- Testi StringExpr:ille.
testExpS :: StringExpr
testExpS = Fx $ (:+) (Fx $ C "kissa ") (Fx $ C "istuu!")

-- Jasennin-osio

-- Jasentimen maarittely.
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
                             Nothing -> runParser b x   
                             r       -> r
-- Kielioppi:
-- E -> T + E | T - E | T
-- T -> P * T | P / T | P
-- P -> (E) | c
expr2 :: Parser DoubleExpr
expr2 = e 
    where e = add <> sub <> t
          t = mult <> div <> p 
          p = par e <> cons  
          cons = fmap Fx $ (pure C) <*> double
          add = fmap Fx $ pure (:+) <*> (t <* string "+") <*> e
          sub = fmap Fx $ pure (:-) <*> (t <* string "-") <*> e
          mult = fmap Fx $ pure (:*) <*> (p <* string "*") <*> t
          div = fmap Fx $ pure (:/) <*> (p <* string "/") <*> t 

-- Ensimmainen evaluointi-funktio. Jos jasennin epaonnistuu tai jos syotetta jaa lukematta, niin palautetaan Nothing. 
-- Jos taas jasennys onnistuu ja syote on luettu loppuun, niin kutsutaan varsinaista jasennys funktiota (cata).
evaluate a s = case runParser a s of
                 Nothing         -> Nothing
                 Just (v,"")     -> Just $ cata evalAlgebra v
                 Just (_,(x:xs)) -> Nothing

runParser :: Parser a -> String -> Maybe (a,String)
runParser (P f) = f

-- Funktio, joka muodostaa kahdesta jasentimesta uuden jasentimen. Toimintalogiikka on seuraava: pa (not)pb. Eli uusi jasennin testaa 
-- sellaista tilannetta, etta vain ensimmainen jasennin onnistuu, ja toisen taytyy epaonnistua. Jos nain kay, niin palautetaan ensimmaisen 
-- jasentimen tulos. Tata ei kayteta tassa ohjelmassa, mutta talla yritettiin saada PEG-kielioppi toimimaan siten, etta ensin testattaisiin 
-- onko numero, mutta sita ei seuraa +-*/ merkkeja. Tama ei kuitenkaan ratkaissut eksponettiaalisen ajan ongelmaa, vaan siirti ko. ongelmaa muualle.
fstNotSnd :: Parser a -> Parser b -> Parser a
fstNotSnd pa pb = P $ \s -> case runParser pa s of
                             Nothing     -> Nothing
                             Just (x,s') -> case runParser pb s' of
                               Nothing -> Just (x,s')
                               _       -> Nothing 

-- Seuraavaksi joukko "primitiivi"-jasentajia, joiden avulla saadaan kasattua monimutkaisemia jasentimia.
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

-- Kontekstiton kielioppi, joka tuottaa doublen:
-- S -> MA | ZA | ZB | d
-- A -> ZA | ZB | d
-- B -> XC | YZ 
-- C -> PF | MF | ZF | d
-- D -> ZD | ZE | f
-- E -> XC
-- F -> ZF | d
-- P -> +
-- M -> -
-- X -> e
-- Y -> .
-- Z -> d
--
-- Pitaisi nyt olla Chomskyn normaalimuodossa. Tosin Haskelissa on hieman oikaistu
-- yksittaisten digit:ien kanssa. Ne pitaisi ainakin teoriassa olla paatemerkkeja
-- eika valikemerkkeja ollessaan ainoana merkkina. Kielioppi ei tarkista
-- sita, onko jasennetty merkkijono lukualueeltaan liian suuri tai pieni mahtumaan
-- doubleen. Ts. voi tulla Infinity tai -Infinity. 
double :: Parser Double
double = P $ \x -> let s = (minus +++ a) <> (digit +++ a) <> (digit +++ b) <> digit
                       a = (digit +++ a) <> (digit +++ b) <> digit
                       b = (ee +++ c) <> (dot +++ d) 
                       c = (plus +++ f) <> (minus +++ f) <> (digit +++ f) <> digit 
                       d = (digit +++ d) <> (digit +++ e) <> f
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

-- Doublen jasentimen testitapaukset. Tassa ei valttamatta ole kaikkia mahdollisia testitatauksia, vaan joitakin tarkeimpia.
doubles :: [String]
doubles = ["1","-1","1874","-1874","18.74","-18.74","18.74e3","18.74e+3","18.74e-3",
           "-18.74e3","-18.74e+3","-18.74e-3","187e3","187e+3","187e-3","-187e3","-187e+3","-187e-3","123.5e3"]

-- Testausfunktio.
doTest :: (String -> String) -> [String] -> IO ()
doTest tFunc inputs = do mapM_ (putStrLn . tFunc) inputs 

-- Doublen jasentimen testausfunktio.
testDoubleFunc s = let x = (runParser double) s 
                       p = case x of
                             Nothing -> "Failed"
                             Just (d,"") -> "Passed"
                             Just (d, (x:xs)) -> "Failed"
                   in ("INPUT = " ++ s ++ " PARSED = " ++ show x ++ " TEST RESULT = " ++ p) 

-- Varsinainen double testaus.
testDouble = doTest testDoubleFunc doubles

-- Funktio, joka evaluoi annetun merkkijonon, ja antaa merkkijonona joko virheilmoituksen tai lausekkeen tuloksen.
check input = case evaluate expr2 input of
                Nothing -> "Syntaksi virhe tai jotain..."
                Just v  -> "Vastaus on " ++ show v

-- "Funktio", joka kysyy kayttajalta syotetta. Jos kayttaja antaa merkkijonon "q",
-- niin palautetaan IO monadin sisalla merkkijonon "quit", muutoin palautetaan kayttajan antama syote.
ask :: IO String
ask = do
        putStr "Anna arimeettien lauseke, joka koostuu merkeista ()+-*/0123456789e. Poistu antamalla kirjain q. > "
        hFlush stdout
        a <- getLine 
        if a == "q" then Prelude.return "quit" else Prelude.return (check a)

-- Ohjelman paasilmukka.
loop :: IO ()
loop = do 
         result <- ask
         if result == "quit" then print "" else putStrLn result
         unless (result == "quit") loop

-- Paaohjelma.
main = do     
          print "Ohjelma yksinkertaisten aritmeettisten lausekkeiden laskemiseen. Author Janne Kauppinen. Copyright: None."
          loop 
