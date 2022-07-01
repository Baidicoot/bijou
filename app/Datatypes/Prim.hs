module Datatypes.Prim where

data Primop
    = Add
    | Sub
    | Div
    | Mult
    | PrintF
    deriving(Show,Eq)

data Lit
    = Int Int
    | String String
    deriving(Eq)

instance Show Lit where
    show (Int i) = show i
    show (String s) = show s