module Main where

import Data.Array
import Data.Maybe
import Data.Tuple
import Data.Char

import Debug.Trace

import Starter.Kit.Example

import Test.QuickCheck


main = do

  trace "Should always succeed"
  quickCheck $ \s x -> parse (return x) s == [Tuple x s] :: [Tuple Number [Char]]

  trace "Should always fail"
  quickCheck $ \s -> parse fail s == [] :: [Tuple Number [Char]]

  trace "Should consume one character"
  quickCheck $ \s -> parse item s == case s of
                                      [] -> []
                                      x:xs -> [Tuple x xs]


