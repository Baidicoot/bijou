{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LAnguage TypeFamilies #-}
module Datatypes.Type where

import Datatypes.Prim
import Datatypes.Name
import Data.Functor.Foldable
import Data.Functor.Foldable.TH

import qualified Data.Foldable as F
import qualified Data.Set as S
import qualified Data.Map as M

import Data.Char (isSpace)

type Substitution = M.Map Name Type

data Type
    = Arr
    | Star
    | App Type Type
    | Const Name
    | TyVar Name
    | PrimTy PrimTy
    | Rigid Name
    deriving(Eq,Ord)

instance Show Type where
    show Star = "*"
    show (App (App Arr a) b) = parens (show a) ++ " -> " ++ show b
        where
            parens s | any isSpace s = "(" ++ s ++ ")"
            parens s = s
    show Arr = "(->)"
    show (App a b) = show a ++ " " ++ parens (show b)
        where
            parens s | any isSpace s = "(" ++ s ++ ")"
            parens s = s
    show (Const n) = show n
    show (TyVar n) = show n
    show (PrimTy p) = show p
    show (Rigid n) = '#':show n

arr :: Type -> Type -> Type
arr = App . App Arr

unarrs :: Type -> ([Type],Type)
unarrs (App (App Arr a) b) = let (as,r) = unarrs b in (a:as,r)
unarrs t = ([],t)

arity :: Type -> Int
arity = length . fst . unarrs

makeBaseFunctor ''Type

instance Subst Type where
    subst g = cata go
        where
            go (TyVarF n) = M.findWithDefault (TyVar n) n g
            go x = embed x

instance Free Type where
    fv = cata go
        where
            go (TyVarF n) = S.singleton n
            go x = F.fold x

data TypeError
    = UnificationFail Type Type
    | UnificationPoly Polytype Polytype
    | RecursiveType Name Type
    | UnknownVar Name
    deriving(Show)

data Polytype = Forall (S.Set Name) Type
    deriving(Eq,Ord)

instance Show Polytype where
    show (Forall s t) | S.null s = show t
    show (Forall s t) = "forall " ++ unwords (fmap show (S.toList s)) ++ ", " ++ show t

substPoly :: M.Map Name Type -> Polytype -> Polytype
substPoly g (Forall s t) = Forall s (subst (foldr M.delete g s) t)

instance Free Polytype where
    fv (Forall s t) = S.difference (fv t) s