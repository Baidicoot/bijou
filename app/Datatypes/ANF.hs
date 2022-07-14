{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LAnguage TypeFamilies #-}
module Datatypes.ANF where

import Datatypes.Prim
import Datatypes.Pattern
import Datatypes.Name
import Datatypes.Type

import Data.Functor.Foldable
import Data.Functor.Foldable.TH

data ANFVal
    = ANFVar Name
    | ANFLabel Name
    | ANFLit Lit
    | ANFThrow String

instance Show ANFVal where
    show (ANFVar n) = show n
    show (ANFLabel n) = '#':show n
    show (ANFLit l) = show l
    show (ANFThrow e) = "throw '" ++ e ++ "'"

data ANFExpr
    = ANFMkClosure Name Name [ANFVal] ANFExpr
    | ANFMkCons Name Name [ANFVal] ANFExpr
    | ANFLet Name ANFVal ANFExpr
    | ANFUnpackPartial [Name] ANFVal ANFExpr
    | ANFAppPartial Name ANFVal ANFVal ANFExpr
    | ANFAppGlobal Name Name [ANFVal] ANFExpr
    | ANFPrimop Name Primop [ANFVal] ANFExpr
    | ANFCCall Name String [ANFVal] ANFExpr
    | ANFMatch ANFVal [(FlatPattern,ANFExpr)] ANFExpr
    | ANFReturn ANFVal
    deriving(Show)

makeBaseFunctor ''ANFExpr

data ANFFunc
    = ANFFunc Name [Name] ANFExpr
    deriving(Show)