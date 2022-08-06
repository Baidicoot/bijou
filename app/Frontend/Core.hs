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

data Core
    = Abs Name Icit Ty Core
    | App Icit Core Core
    | Prod Name Icit Ty Core
    | Cast Core Ty
    | Lit PrimLit
    | Var Name
    | Cons Name
    | Meta MetaName
    | Let Name Ty Core Core
    | LetRec [(Name,Ty,Core)] Core
    | Match [(CorePattern,Core)]
    | Primop Primop [Core]
    deriving(Eq,Show)

infixl 4 :>
pattern xs :> x = x:xs

type Cls = (M.Map Name Val,R.Raw)

type Spine v = [(Icit,v)]

data Val
    = VRigid Name (Spine Val)
    | VFlex MetaName (Spine Val)
    | VCons Name (Spine Val)
    | VPrimop Primop (Spine Val)
    | VLit PrimLit
    | VType
    | VAbs Name Icit Cls
    | VProd Name Icit Val Cls
    deriving(Show)