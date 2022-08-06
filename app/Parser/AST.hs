{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LAnguage TypeFamilies #-}
module Parser.AST where

import Datatypes.Prim
import Data.Functor.Foldable
import Data.Functor.Foldable.TH

import Data.Bifunctor

data Ident
    = Qualified String String
    | Unqualified String
    | Hole
    deriving(Show,Eq,Ord)

data ASTData
    = ASTGADT Ident [(Ident,ASTPoly)]
    | ASTADT Ident [Ident] [(Ident,[ASTType])]
    | ASTStruct Ident [Ident] Ident [(Ident,ASTType)]
    deriving(Show)

astDataType :: ASTData -> Ident
astDataType (ASTGADT i _) = i
astDataType (ASTADT i _ _) = i
astDataType (ASTStruct i _ _ _) = i

astDataConsNames :: ASTData -> [Ident]
astDataConsNames (ASTGADT _ cs) = fmap fst cs
astDataConsNames (ASTADT _ _ cs) = fmap fst cs
astDataConsNames (ASTStruct _ _ c _) = [c]

astDataCons :: ASTData -> [(Ident,ASTPoly)]
astDataCons (ASTADT t a cs) =
    fmap (second (\ts -> ASTPolyExplicit a (foldr astArr (foldl ASTTyApp (ASTTyVar t) (fmap ASTTyVar a)) ts))) cs
astDataCons (ASTStruct t a c ts) =
    [(c,ASTPolyExplicit a (foldr astArr (foldl ASTTyApp (ASTTyVar t) (fmap ASTTyVar a)) (fmap snd ts)))]
astDataCons (ASTGADT t cs) = cs

data ASTTLQual
    = ASTExtern Ident Int
    | ASTExport [Ident]
    | ASTEntry Ident
    deriving(Show)

astArr :: ASTType -> ASTType -> ASTType
astArr a b = ASTTyApp (ASTTyApp ASTArr a) b

data ASTType
    = ASTTyVar Ident
    | ASTStar
    | ASTTyApp ASTType ASTType
    | ASTWeakVar Ident
    | ASTArr
    | ASTPrimTy PrimTy
    deriving(Show)

data ASTPoly
    = ASTPolyImplicit ASTType
    | ASTPolyExplicit [Ident] ASTType
    deriving(Show)

data ASTPattern
    = ASTPatApp Ident [ASTPattern]
    | ASTPatLit Lit
    deriving(Show)

makeBaseFunctor ''ASTType

type ASTDefn = (Ident,[Ident],ASTExpr)

data ASTExpr
    = ASTLam [Ident] ASTExpr
    | ASTLet [(Ident,ASTExpr)] ASTExpr
    | ASTLetRec [Either (Ident,ASTPoly) (Ident,[Ident],ASTExpr)] ASTExpr
    | ASTLit Lit
    | ASTAnnot ASTExpr ASTPoly
    | ASTMatch ASTExpr [(ASTPattern,ASTExpr)]
    | ASTApp ASTExpr ASTExpr
    | ASTVar Ident
    | ASTPrimop Primop [ASTExpr]
    | ASTCCall String [ASTExpr]
    | ASTDoThen [ASTExpr] ASTExpr
    deriving(Show)

makeBaseFunctor ''ASTExpr

data ASTTL
    = ASTData ASTData
    | ASTDecl Ident ASTPoly
    | ASTFunc ASTDefn
    | ASTQual ASTTLQual
    | ASTEmbC String
    deriving(Show)