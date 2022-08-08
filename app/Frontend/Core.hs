{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DerivingVia #-}
module Frontend.Core where

import Univ.Name

import Frontend.Common
import qualified Frontend.Raw as R

import qualified Data.Map as M

type Ty = (Usage,Core)

newtype MetaName = MetaName {unMeta :: Int} deriving(Eq,Show,Ord) via Int

data CorePattern
    = Uncons Name [(Icit,CorePattern)]
    | MatchLit PrimLit
    | BindVar Name Ty
    deriving(Eq,Show)

-- usage annotations on binding sites only?
data Core
    = Abs Name Icit Ty Core
    | App Icit Core Core
    | Prod Name Icit Ty Core
    | Cast Core Ty
    | Var Name
    | Cons Name
    | Meta MetaName
    | Let Name Ty Core Core
    | LetRec [(Name,Ty,Core)] Core
    | Match [(CorePattern,Core)]
    | PrimTy PrimTy
    | PrimVal PrimLit
    | Primop Primop [Core]
    deriving(Eq,Show)

infixl 4 :>
pattern xs :> x = x:xs

type Cls = (M.Map Name Val,R.Raw)

type Spine v = [(Icit,v)]

type VTy = (Usage,Val)

data Val
    = VRigid Name (Spine Val)
    | VTop Name (Spine Val) Val
    | VFlex MetaName (Spine Val)
    | VCons Name (Spine Val)
    | VType
    | VAbs Name Icit Cls
    | VAttr Val Usage
    | VProd Name Icit VTy Cls
    | VPrimTy PrimTy
    | VPrimLit PrimLit
    | VPrimop Primop [Val]
    deriving(Show)