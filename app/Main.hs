{-# LANGUAGE OverloadedStrings #-}
module Main where

import Datatypes.Lam
import Datatypes.Prim
import ClosureConv

foo :: LamExp
foo = Lam [User "x" 0] (Let (User "addx" 0) (Lam [User "y" 0] (App (App (Primop Add) (Var $ User "x" 0)) (Var $ User "y" 0))) (App (Var $ User "addx" 0) (Var $ User "z" 0)))

main :: IO ()
main = print (cconv 0 foo)
