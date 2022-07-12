{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LAnguage TypeFamilies #-}
module Datatypes.Core where

import Datatypes.Prim
import Datatypes.Name
import Datatypes.Type
import Datatypes.Pattern

import Data.Functor.Foldable
import Data.Functor.Foldable.TH

import qualified Data.Foldable as F
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import qualified Data.Map as M

data CoreExpr
    = CoreLam Name CoreExpr
    | CoreApp CoreExpr CoreExpr
    | CoreLet Name CoreExpr CoreExpr
    | CoreLetRec [(Name,CoreExpr)] CoreExpr
    | CoreVar Name
    | CoreCons Name
    | CoreLit Lit
    | CorePrimop Primop [CoreExpr]
    | CoreCCall String [CoreExpr]
    | CoreMatch CoreExpr [(Pattern,CoreExpr)]
    | CoreAnnot CoreExpr Polytype
    | CoreHole

makeBaseFunctor ''CoreExpr

instance Free CoreExpr where
    fv = cata go
        where
            go (CoreLamF n e) = S.delete n e
            go (CoreLetF n a b) = S.union a (S.delete n b)
            go (CoreLetRecF d e) =
                let
                    f = S.fromList (fmap fst d)
                    v = e:fmap snd d
                in S.difference (S.unions v) f
            go (CoreVarF n) = S.singleton n
            go (CoreMatchF x ps) = S.union x (S.unions (fmap (\(p,v) -> S.difference v (binds p)) ps))
            go x = F.fold x

data CoreADT
    = CoreADT Name [(Name,Polytype)]

consArities :: CoreADT -> M.Map Name Int
consArities (CoreADT _ defs) = M.fromList (fmap (\(n,Forall _ t) -> (n,arity t)) defs)

consTypes :: CoreADT -> M.Map Name Polytype
consTypes (CoreADT _ defs) = M.fromList defs

consTags :: CoreADT -> M.Map Name Int
consTags (CoreADT _ defs) = M.fromList (zipWith ((,) . fst) defs [0..])

consNames :: CoreADT -> S.Set Name
consNames (CoreADT _ defs) = S.fromList (fmap fst defs)

data CoreMod
    = CoreMod (Maybe Name) [Name] [CoreADT] [(Name,Polytype)] [(Name,CoreExpr)]

consTagsTL :: CoreMod -> M.Map Name Int
consTagsTL (CoreMod _ _ d _ _) = M.unions (fmap consTags d)

defsTL :: CoreMod -> [(Name,CoreExpr)]
defsTL (CoreMod _ _ _ _ f) = f

globalsTL :: CoreMod -> S.Set Name
globalsTL (CoreMod _ _ d e f) = S.unions (S.fromList (fmap fst e):S.fromList (fmap fst f):fmap consNames d)

typedTL :: CoreMod -> M.Map Name Polytype
typedTL (CoreMod _ _ d e f) = M.unions (M.fromList e:M.fromList t:fmap consTypes d)
    where
        t = mapMaybe (\(n,e) -> case e of
            CoreAnnot _ p -> Just (n,p)
            _ -> Nothing) f