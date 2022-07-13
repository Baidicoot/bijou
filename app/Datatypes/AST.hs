{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LAnguage TypeFamilies #-}
module Datatypes.AST where

import Datatypes.Prim
import Data.Functor.Foldable
import Data.Functor.Foldable.TH

data Ident
    = Qualified String String
    | Unqualified String
    | Hole
    deriving(Eq,Ord)

data ASTData
    = ASTGADT Ident [(Ident,ASTType)]
    | ASTADT Ident [Ident] [(Ident,[ASTType])]
    | ASTStruct Ident [Ident] Ident [(Ident,ASTType)]

data ASTTLQual
    = ASTExtern
    | ASTExport
    | ASTEntry

astArr :: ASTType -> ASTType -> ASTType
astArr a b = ASTTyApp (ASTTyApp ASTArr a) b

data ASTType
    = ASTTyVar Ident
    | ASTStar
    | ASTTyApp ASTType ASTType
    | ASTArr
    | ASTPrimTy PrimTy

data ASTPattern
    = ASTPatApp Ident [ASTPattern]
    | ASTPatLit Lit

makeBaseFunctor ''ASTType

type ASTDefn = (Ident,[Ident],ASTExpr)

data ASTExpr
    = ASTLam [Ident] ASTExpr
    | ASTLet [(Ident,ASTExpr)] ASTExpr
    | ASTLetRec [Either (Ident,ASTType) (Ident,[Ident],ASTExpr)] ASTExpr
    | ASTLit Lit
    | ASTAnnot ASTExpr ASTType
    | ASTMatch ASTExpr [(ASTPattern,ASTExpr)]
    | ASTApp ASTExpr ASTExpr
    | ASTVar Ident
    | ASTPrimop Primop [ASTExpr]
    | ASTCCall String [ASTExpr]

makeBaseFunctor ''ASTExpr

data ASTTL
    = ASTData ASTData
    | ASTDecl Ident ASTType
    | ASTFunc ASTDefn
    | ASTQual Ident ASTTLQual