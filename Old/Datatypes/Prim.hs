module Datatypes.Prim where

data Primop
    = Add
    | Sub
    | Div
    | Mul
    deriving(Show,Eq)

boolop :: Primop -> Bool
boolop Add = True
boolop Sub = True
boolop Div = True
boolop Mul = True

data Lit
    = IntLit Int
    | StrLit String
    deriving(Eq)

data PrimTy
    = IntTy
    | StrTy
    deriving(Eq,Show,Ord)

instance Show Lit where
    show (IntLit i) = show i
    show (StrLit s) = show s