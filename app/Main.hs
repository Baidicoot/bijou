{-# LANGUAGE OverloadedStrings #-}
module Main where

import Datatypes.Lam
import Datatypes.Prim
import ClosureConv
import PartialApp
import Parser

str :: String
str = "(\\a b c -> a c (b c)))"

main :: IO ()
main = print (parseLamExpr "" str)