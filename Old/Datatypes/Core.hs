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

import Data.Bifunctor (first,second)
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
    deriving(Show)

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
    deriving(Show)

consArities :: CoreADT -> M.Map Name Int
consArities (CoreADT _ defs) = M.fromList (fmap (\(n,Forall _ t) -> (n,arity t)) defs)

consTypes :: CoreADT -> M.Map Name Polytype
consTypes (CoreADT _ defs) = M.fromList defs

data Repr
    = Enum Int
    | Struct
    | Newtype
    | Standard Int
    | Const Int

consRepr :: CoreADT -> M.Map Name Repr
consRepr (CoreADT _ cs) | all (\(_,Forall _ t) -> arity t==0) cs = M.fromList (fmap (\((c,_),i)->(c,Enum i)) (zip cs [0..]))
consRepr (CoreADT _ [(c,Forall _ t)]) | arity t == 1 = M.singleton c Newtype
consRepr (CoreADT _ [(c,_)]) = M.singleton c Struct
consRepr (CoreADT _ cs) = M.fromList (fmap (\((c,Forall _ t),i) -> case arity t of
    0 -> (c,Const i)
    _ -> (c,Standard i)) (zip cs [0..]))

consNames :: CoreADT -> S.Set Name
consNames (CoreADT _ defs) = S.fromList (fmap fst defs)

typeName :: CoreADT -> Name
typeName (CoreADT t _) = t

data CoreModExports = Exports
    { cons :: [Name]
    , types :: [Name]
    , funcs :: [Name]
    }
    deriving(Show)

exportNames :: CoreModExports -> S.Set Name
exportNames (Exports c t f) = S.fromList (c ++ t ++ f)

data CoreMod
    = CoreMod (Maybe Name) String CoreModExports [CoreADT] [(Name,(Int,Polytype))] [(Name,CoreExpr)]
    deriving(Show)

datasMod :: CoreMod -> [CoreADT]
datasMod (CoreMod _ _ _ d _ _) = d

consReprTL :: CoreMod -> M.Map Name Repr
consReprTL (CoreMod _ _ _ d _ _) = M.unions (fmap consRepr d)

defsTL :: CoreMod -> [(Name,CoreExpr)]
defsTL (CoreMod _ _ _ _ _ f) = f

globalsTL :: CoreMod -> S.Set Name
globalsTL (CoreMod _ _ _ d e f) = S.unions (S.fromList (fmap fst e):S.fromList (fmap fst f):fmap consNames d)

typedTL :: CoreMod -> M.Map Name Polytype
typedTL (CoreMod _ _ _ d e f) = M.unions (M.fromList (fmap (second snd) e):M.fromList t:fmap consTypes d)
    where
        t = mapMaybe (\(n,e) -> case e of
            CoreAnnot _ p -> Just (n,p)
            _ -> Nothing) f

typedTLWithoutDefs :: CoreMod -> M.Map Name Polytype
typedTLWithoutDefs (CoreMod _ _ _ d e _) = M.unions (M.fromList (fmap (second snd) e):fmap consTypes d)

exportNamesTL :: CoreMod -> CoreModExports
exportNamesTL (CoreMod _ _ x _ _ _) = x

aritiesTL :: CoreMod -> M.Map Name Int
aritiesTL (CoreMod _ _ ep d ex _) = M.unions (M.fromList (fmap (second fst) ex):fmap consArities d)

startName :: CoreMod -> Maybe Name
startName (CoreMod st _ _ _ _ _) = st

embeddedC :: CoreMod -> String
embeddedC (CoreMod _ c _ _ _ _) = c