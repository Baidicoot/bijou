{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LAnguage TypeFamilies #-}
module Datatypes.Pattern where

import Datatypes.Prim
import Datatypes.Name
import Datatypes.Type

import Data.Functor.Foldable
import Data.Functor.Foldable.TH

import qualified Data.Set as S
import qualified Data.Foldable as F

data Pattern
    = PatternApp Name [Pattern]
    | PatternVar Name
    | PatternLit Lit
    deriving(Show,Eq)

makeBaseFunctor ''Pattern

binds :: Pattern -> S.Set Name
binds = cata go
    where
        go (PatternVarF n) = S.singleton n
        go x = F.fold x

data FlatPattern
    = FlatPatternApp Name [Name]
    | FlatPatternLit Lit
    deriving(Eq,Show)