{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LAnguage TypeFamilies #-}
module Datatypes.Lifted where

import Datatypes.Prim
import Datatypes.Name
import Datatypes.Type
import Datatypes.Pattern

import Data.Functor.Foldable
import Data.Functor.Foldable.TH

data LiftedExpr
    = LiftedApp LiftedExpr LiftedExpr
    | LiftedLet Name LiftedExpr LiftedExpr
    | LiftedVar Name
    | LiftedLit Lit
    | LiftedCons Name
    | LiftedPrimop Primop [LiftedExpr]
    | LiftedCCall String [LiftedExpr]
    | LiftedThrow String
    | LiftedMatch LiftedExpr [(FlatPattern,LiftedExpr)] LiftedExpr
    deriving(Show)

data LiftedDef
    = LiftedDef Name [Name] LiftedExpr

makeBaseFunctor ''LiftedExpr