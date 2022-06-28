{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LAnguage TypeFamilies #-}
module Datatypes.Lam where

import Datatypes.Prim

import qualified Data.Text as T
import qualified Data.Set as S
import Data.Functor.Foldable
import Data.Functor.Foldable.TH

data Name = User T.Text Int | Gen Int
    deriving(Eq, Ord, Show)

class Free a where
    fv :: a -> S.Set Name

data Def x = Def Name [Name] x deriving(Functor, Foldable, Traversable, Show)

data CoreExpr
    = CoreLam [Name] CoreExpr
    | CoreApp CoreExpr CoreExpr
    | CoreLet Name CoreExpr CoreExpr
    | CoreLetRec [Def CoreExpr] CoreExpr
    | CoreVar Name
    | CorePrimop Primop
    deriving(Show)

makeBaseFunctor ''CoreExpr

instance Free CoreExpr where
    fv = cata go
        where
            go (CoreLamF n e) = S.difference e (S.fromList n)
            go (CoreAppF a b) = S.union a b
            go (CoreLetF n a b) = S.union a (S.delete n b)
            go (CoreLetRecF d e) =
                let
                    f = S.fromList (fmap (\(Def n _ _) -> n) d)
                    v = e:fmap (\(Def _ n s) -> S.difference s (S.fromList n)) d
                in S.difference (S.unions v) f
            go (CoreVarF n) = S.singleton n
            go _ = S.empty

data LiftedExpr
    = LiftedApp LiftedExpr LiftedExpr
    | LiftedLet Name LiftedExpr LiftedExpr
    | LiftedVar Name
    | LiftedPrimop Primop
    deriving(Show)

makeBaseFunctor ''LiftedExpr

data NoPartialsExpr
    = NoPartialsAppPartial NoPartialsExpr NoPartialsExpr
    | NoPartialsAppGlobal Name [NoPartialsExpr]
    | NoPartialsMkPartial Name [NoPartialsExpr]
    | NoPartialsUnpackPartial [Name] NoPartialsExpr NoPartialsExpr
    | NoPartialsLet Name NoPartialsExpr NoPartialsExpr
    | NoPartialsVar Name
    | NoPartialsLabel Name
    | NoPartialsPrimop Primop
    deriving(Show)

makeBaseFunctor ''NoPartialsExpr

{-
data ANFVal
    = VarA Name
    | LabelA Name
    deriving(Eq, Show)

data ANFExp
    = MkClosureA Name [ANFVal] ANFExp
    | LetA Name ANFExp ANFExp
    | AppPartialA Name ANFVal
    | AppGlobalA Name [ANFVal]
    | ReturnA ANFVal
    deriving(Eq, Show)
-}