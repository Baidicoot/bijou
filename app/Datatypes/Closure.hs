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
    = ClosureAppLocal ClosureExpr [ClosureExpr]
    | ClosureAppGlobal Name [ClosureExpr]
    | ClosureMkRecord [ClosureExpr]
    -- | ClosureUnpackRecord [Name] ClosureExpr ClosureExpr
    | ClosureIndexRecord Name Int ClosureExpr ClosureExpr
    | ClosureLet Name ClosureExpr ClosureExpr
    | ClosureVar Name
    | ClosureLabel Name
    | ClosureRef Name
    | ClosureLit Lit
    | ClosurePrimop Primop [ClosureExpr]
    | ClosureCCall String [ClosureExpr]
    | ClosureThrow String
    | ClosureSwitch ClosureExpr [(Lit,ClosureExpr)] ClosureExpr
    -- | ClosureMatch ClosureExpr [(FlatPattern,ClosureExpr)] ClosureExpr
    deriving(Show)

data ClosureFunc
    = ClosureFunc Name [Name] ClosureExpr
    | ClosureConst Name Lit
    deriving(Show)

makeBaseFunctor ''ClosureExpr