module Starter.Kit.Example where

import Data.Char
import Data.Array
import Data.Maybe
import Data.Tuple

newtype Parser a = P ([Char] -> [Tuple a [Char]])

parse :: forall a. Parser a -> [Char] -> [Tuple a [Char]]
parse (P p) s = p s

fail :: forall a. Parser a
fail = P (\s -> [])

item :: Parser Char
item = P (\s -> case s of
                     [] -> []
                     x:xs -> [Tuple x xs]) 

instance functorParsers :: Functor Parser where
--(<$>) :: forall a b. (a -> b) -> f a -> f b
  (<$>) f m =  P (\s -> case (parse m s) of
  	                      [] -> []
  	                      [Tuple v s'] -> [Tuple (f v)  s'])

instance applyParsers :: Apply Parser where
--(<*>) :: forall a b. f (a -> b) -> f a -> f b
  (<*>) mf m = P(\s -> case (parse mf s) of
  	                      [] -> []
  	                      [Tuple f s'] -> case (parse m s') of
  	                                        [] -> []
  	                                        [Tuple v s''] -> [Tuple (f v) s''])

instance applicativeParsers :: Applicative Parser where
--pure :: forall a. a -> f a
  pure x = P (\s -> [Tuple x s]) 

instance bindParsers :: Bind Parser where
  (>>=) m f = P(\s -> case (parse m s) of
  	                    [] -> []
  	                    [Tuple v s'] -> parse (f v) s')

instance monadParsers :: Monad Parser 


sat :: (Char -> Boolean) -> Parser Char
sat p = do x <- item
           if (p x) then return x else fail