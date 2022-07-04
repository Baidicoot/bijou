module Datatypes.Prim where

data Primop
    = Add
    | Sub
    | Div
    | Mult
    | PrintF
    deriving(Show,Eq)

data Lit
    = IntLit Int
    | StrLit String
    deriving(Eq)

data PrimTy
    = IntTy
    | StrTy
    deriving(Eq,Show)

instance Show Lit where
    show (IntLit i) = show i
    show (StrLit s) = show s