module Frontend.Common where

data Icit
    = Impl
    | Expl
    deriving(Eq,Show,Ord)

data Mult
    = Linear
    | Affine
    | Zero
    | Omega
    deriving(Eq,Show)

data Usage = Usage {erased :: Mult, runtime :: Mult} deriving(Eq,Show)

data PrimLit
    = IntLit Int
    | StrLit String
    deriving(Eq,Show)

data Primop
    = Mul
    | Div
    | Add
    | Sub
    deriving(Eq,Show)