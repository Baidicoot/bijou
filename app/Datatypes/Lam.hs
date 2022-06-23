{-# LANGUAGE ApplicativeDo #-}
module Datatypes.Lam where

import Datatypes.Prim

import qualified Data.Text as T
import qualified Data.Set as S

data Name = User T.Text Int | Gen Int
    deriving(Eq, Ord, Show)

class Free a where
    fv :: a -> S.Set Name

data LamExp
    = Lam [Name] LamExp
    | App LamExp LamExp
    | Let Name LamExp LamExp
    | LetRec [LamDef] LamExp
    | Var Name
    | Primop Primop
    deriving(Eq, Show)

data LiftExp
    = AppL LiftExp LiftExp
    | LetL Name LiftExp LiftExp
    | VarL Name
    | PrimopL Primop
    deriving(Eq, Show)

data PartialExp
    = AppPartialP PartialExp PartialExp
    | AppGlobalP Name [PartialExp]
    | MkPartialP Name [PartialExp]
    | UnpackP [Name] PartialExp PartialExp
    | LetP Name PartialExp PartialExp
    | VarP Name
    | LabelP Name
    | PrimopP Primop
    deriving(Eq, Show)

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

instance Free LamExp where
    fv (Lam n e) = foldr S.delete (fv e) n
    fv (App a b) = S.union (fv a) (fv b)
    fv (Let n a b) = S.union (fv a) (S.delete n (fv b))
    fv (Var n) = S.singleton n
    fv _ = S.empty

data Def x = Def Name [Name] x deriving(Eq,Show)
type LamDef = Def LamExp
type DefL = Def LiftExp
type DefP = Def PartialExp