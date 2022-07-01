{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LAnguage TypeFamilies #-}
module Datatypes.Lam where

import Datatypes.Prim

import qualified Data.Set as S
import Data.Functor.Foldable
import Data.Functor.Foldable.TH

data Name = User String Int | Gen Int
    deriving(Eq, Ord)

instance Show Name where
    show (User n 0) = n
    show (User n i) = n ++ ".." ++ show i
    show (Gen i) = "v." ++ show i

class Free a where
    fv :: a -> S.Set Name

data Def x = Def Name [Name] x deriving(Functor, Foldable, Traversable)

instance Show x => Show (Def x) where
    show (Def f a x) = "def " ++ show f ++ show a ++ "\n" ++ show x

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
                    f = S.fromList (fmap (\(Def n _ _) -> n) d)
                    v = e:fmap (\(Def _ n s) -> S.difference s (S.fromList n)) d
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