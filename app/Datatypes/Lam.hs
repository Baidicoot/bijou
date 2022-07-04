{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LAnguage TypeFamilies #-}
module Datatypes.Lam where

import Datatypes.Prim
import Datatypes.Name

import Data.Functor.Foldable
import Data.Functor.Foldable.TH

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
    | Rigid Name
    | PrimTy PrimTy
    deriving(Eq)

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
    show (Rigid n) = "#" ++ show n
    show (PrimTy p) = show p

arr :: Type -> Type -> Type
arr = App . App Arr

makeBaseFunctor ''Type

instance Free Type where
    fv = cata go
        where
            go (TyVarF n) = S.singleton n
            go (AppF a b) = S.union a b
            go x = S.empty

class Subst a where
    subst :: Substitution -> a -> a

instance Subst Type where
    subst g = cata go
        where
            go (TyVarF n) = M.findWithDefault (TyVar n) n g
            go x = embed x

data TypeError
    = UnificationFail Type Type
    | RecursiveType Name Type
    | UnknownVar Name
    deriving(Show)

data Polytype = Forall (S.Set Name) Type deriving(Eq)

instance Show Polytype where
    show (Forall s t) = "forall " ++ unwords (fmap show (S.toList s)) ++ ", " ++ show t

rigidify :: Polytype -> Type
rigidify (Forall n t) = subst (M.fromSet Rigid n) t

data Def x = Def Name [Name] (Maybe Polytype) x deriving(Functor, Foldable, Traversable)

instance Show x => Show (Def x) where
    show (Def f a (Just t) x) = "dec " ++ show f ++ " :: " ++ show t ++ "\n" ++ show (Def f a Nothing x)
    show (Def f a _ x) = "def " ++ show f ++ show a ++ "\n" ++ show x

data CoreExpr
    = CoreLam [Name] CoreExpr
    | CoreApp CoreExpr CoreExpr
    | CoreLet Name CoreExpr CoreExpr
    | CoreLetRec [Def CoreExpr] CoreExpr
    | CoreVar Name
    | CoreLit Lit
    | CorePrimop Primop [CoreExpr]
    | CoreCCall String [CoreExpr]
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
                    f = S.fromList (fmap (\(Def n _ _ _) -> n) d)
                    v = e:fmap (\(Def _ n _ s) -> S.difference s (S.fromList n)) d
                in S.difference (S.unions v) f
            go (CoreVarF n) = S.singleton n
            go (CorePrimopF p a) = S.unions a
            go x = S.empty

data LiftedExpr
    = LiftedApp LiftedExpr LiftedExpr
    | LiftedLet Name LiftedExpr LiftedExpr
    | LiftedVar Name
    | LiftedLit Lit
    | LiftedPrimop Primop [LiftedExpr]
    | LiftedCCall String [LiftedExpr]
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
    | NoPartialsLit Lit
    | NoPartialsPrimop Primop [NoPartialsExpr]
    | NoPartialsCCall String [NoPartialsExpr]
    deriving(Show)

makeBaseFunctor ''NoPartialsExpr

data ANFVal
    = ANFVar Name
    | ANFLabel Name
    | ANFLit Lit

instance Show ANFVal where
    show (ANFVar n) = show n
    show (ANFLabel n) = '#':show n
    show (ANFLit l) = show l

data ANFExpr
    = ANFMkClosure Name Name [ANFVal] ANFExpr
    | ANFLet Name ANFVal ANFExpr
    | ANFUnpackPartial [Name] ANFVal ANFExpr
    | ANFAppPartial Name ANFVal ANFVal ANFExpr
    | ANFAppGlobal Name Name [ANFVal] ANFExpr
    | ANFPrimop Name Primop [ANFVal] ANFExpr
    | ANFCCall Name String [ANFVal] ANFExpr
    | ANFReturn ANFVal

makeBaseFunctor ''ANFExpr

instance Show ANFExpr where
    show = cata go
        where
            go (ANFMkClosureF r l v k) = show r ++ " <- " ++ show l ++ "@" ++ show v ++ "\n" ++ k
            go (ANFLetF r v k) = show r ++ " <- " ++ show v ++ "\n" ++ k
            go (ANFUnpackPartialF r v k) = show r ++ " <- " ++ show v ++ "\n" ++ k
            go (ANFAppPartialF r f a k) = show r ++ " <- " ++ show f ++ "(" ++ show a ++ ")\n" ++ k
            go (ANFAppGlobalF r f a k) = show r ++ " <- #" ++ show f ++ show a ++ "\n" ++ k
            go (ANFReturnF v) = "return " ++ show v
            go (ANFPrimopF r p a k) = show r ++ " <- " ++ show p ++ show a ++ "\n" ++ k
            go (ANFCCallF r f a k) = show r ++ " <- ccall<" ++ f ++ show a ++ ">\n" ++ k