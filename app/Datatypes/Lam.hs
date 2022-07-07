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

instance Subst Polytype where
    subst g (Forall s t) = Forall s (subst (foldr M.delete g s) t)

instance Free Polytype where
    fv (Forall s t) = S.difference (fv t) s

rigidify :: Polytype -> Type
rigidify (Forall n t) = subst (M.fromSet Rigid n) t

data Def x = Def Name [Name] (Maybe Polytype) x deriving(Functor, Foldable, Traversable)

instance Show x => Show (Def x) where
    show (Def f a (Just t) x) = "dec " ++ show f ++ " :: " ++ show t ++ "\n" ++ show (Def f a Nothing x)
    show (Def f a _ x) = "def " ++ show f ++ show a ++ "\n" ++ show x

data Pattern
    = PatternVar Name
    | PatternApp Name [Pattern]
    | PatternLit Lit
    deriving(Eq,Show)

makeBaseFunctor ''Pattern

data FlatPattern
    = FlatPatternApp Name [Name]
    | FlatPatternLit Lit
    deriving(Eq,Show)

data CoreExpr
    = CoreLam [Name] CoreExpr
    | CoreApp CoreExpr CoreExpr
    | CoreLet Name CoreExpr CoreExpr
    | CoreLetRec [Def CoreExpr] CoreExpr
    | CoreVar Name
    | CoreLit Lit
    | CorePrimop Primop [CoreExpr]
    | CoreCCall String [CoreExpr]
    | CoreMatch CoreExpr [(Pattern,CoreExpr)]
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

data TypedExpr
    = TypedLam Name TypedExpr
    | TypedLetRec [(Name,Maybe Polytype,TypedExpr)] TypedExpr
    | TypedApp TypedExpr TypedExpr
    | TypedLit Lit
    | TypedVar Name
    | TypedPrimop Primop [TypedExpr]
    | TypedCCall String [TypedExpr]
    | TypedMatch TypedExpr [(Pattern,TypedExpr)]
    deriving(Show)

makeBaseFunctor ''TypedExpr

instance Free TypedExpr where
    fv = cata go
        where
            go (TypedLamF n e) = S.delete n e
            go (TypedLetRecF n e) = S.unions (S.difference e (S.fromList (fmap (\(x,_,_)->x) n))
                :fmap (\(_,_,x)->x) n)
            go (TypedVarF n) = S.singleton n
            go x = S.unions x

desugar :: CoreExpr -> TypedExpr
desugar = cata go
    where
        go (CoreLamF n e) = foldr TypedLam e n
        go (CoreLetRecF d e) = TypedLetRec (fmap (\(Def f a t e) -> (f,t,foldr TypedLam e a)) d) e
        go (CoreAppF a b) = TypedApp a b
        go (CorePrimopF p e) = TypedPrimop p e
        go (CoreCCallF f e) = TypedCCall f e
        go (CoreVarF n) = TypedVar n
        go (CoreLitF l) = TypedLit l
        go (CoreLetF n x e) = TypedLetRec [(n,Nothing,x)] e
        go (CoreMatchF v x) = TypedMatch v x

desugarDef :: Def CoreExpr -> (Name,Maybe Polytype,TypedExpr)
desugarDef (Def f a t e) = (f,t,foldr TypedLam (desugar e) a)

data LiftedExpr
    = LiftedApp LiftedExpr LiftedExpr
    | LiftedLet Name LiftedExpr LiftedExpr
    | LiftedVar Name
    | LiftedLit Lit
    | LiftedPrimop Primop [LiftedExpr]
    | LiftedCCall String [LiftedExpr]
    | LiftedThrow String
    | LiftedMatch LiftedExpr [(FlatPattern,LiftedExpr)] LiftedExpr
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
    | NoPartialsThrow String
    | NoPartialsMatch NoPartialsExpr [(FlatPattern,NoPartialsExpr)] NoPartialsExpr
    deriving(Show)

makeBaseFunctor ''NoPartialsExpr

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