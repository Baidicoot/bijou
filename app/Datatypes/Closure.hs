{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LAnguage TypeFamilies #-}
module Datatypes.Closure where

import Datatypes.Prim
import Datatypes.Name
import Datatypes.Type
import Datatypes.Pattern

import Data.Functor.Foldable
import Data.Functor.Foldable.TH

data ClosureExpr
    = ClosureAppPartial ClosureExpr ClosureExpr
    | ClosureAppGlobal Name [ClosureExpr]
    | ClosureMkPartial Name [ClosureExpr]
    | ClosureMkCons Name [ClosureExpr]
    | ClosureUnpackPartial [Name] ClosureExpr ClosureExpr
    | ClosureLet Name ClosureExpr ClosureExpr
    | ClosureVar Name
    | ClosureLabel Name
    | ClosureLit Lit
    | ClosurePrimop Primop [ClosureExpr]
    | ClosureCCall String [ClosureExpr]
    | ClosureThrow String
    | ClosureMatch ClosureExpr [(FlatPattern,ClosureExpr)] ClosureExpr
    deriving(Show)

data ClosureFunc
    = ClosureFunc Name [Name] ClosureExpr
    deriving(Show)

makeBaseFunctor ''ClosureExpr