{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DerivingVia #-}
module Frontend.Core where

import qualified Frontend.Raw as R
import Frontend.Common
import Univ.Name

import Control.Monad.State.Lazy
import Control.Monad.Except
import Control.Monad.ST.Lazy

import qualified Data.Vector.Mutable as V
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

type MetaState s = V.MVector s (Maybe (Val s))

data TypeError s
    = NonFunctionApp (Val s) (Val s)
    | NonVarSpine (Val s)
    | CantInvert (Val s)
    | Occurs MetaName

data EvalCtx s
    = EvalCtx
    { recDefs :: M.Map Name (Val s)
    , varDefs :: M.Map Name (Val s)
    }

addVar :: Name -> Val s -> EvalCtx s -> EvalCtx s
addVar n v ctx = ctx {varDefs = M.insert n v (varDefs ctx)}

addRec :: Name -> Val s -> EvalCtx s -> EvalCtx s
addRec n v ctx = ctx {recDefs = M.insert n v (recDefs ctx)}

addRecs :: [(Name,Val s)] -> EvalCtx s -> EvalCtx s
addRecs ns ctx = ctx {recDefs = M.union (M.fromList ns) (recDefs ctx)}

type ElabNoExcept s = StateT (Int,MetaState s) (ST s)
type Elab s = ExceptT (TypeError s) (ElabNoExcept s)

newtype Cls s = Cls {appCls :: Val s -> ElabNoExcept s (Val s)}

instance Show (Cls s) where
    show _ = "(\\x -> ...)"

type Spine v = [(Icit,v)]

type VTy s = (Usage,Val s)

data Val s
    = VRigid Name (Spine (Val s))
    | VTop Name (Spine (Val s)) (Val s)
    | VFlex MetaName (Spine (Val s))
    | VCons Name (Spine (Val s))
    | VType
    | VAbs Name Icit (Cls s)
    | VAttr (Val s) Usage
    | VProd Name Icit (VTy s) (Cls s)
    | VPrimTy PrimTy
    | VPrimLit PrimLit
    | VPrimop Primop [Val s]
    deriving(Show)