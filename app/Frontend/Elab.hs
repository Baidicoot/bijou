module Frontend.Elab where

import qualified Frontend.Raw as R
import Frontend.Core
import Frontend.Common
import Univ.Name

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.ST

import qualified Data.Vector.Mutable as V
import qualified Data.Map as M

type MetaState s = V.MVector s (Maybe Val)

data TypeError = NonFunctionApp Val Val

type Elab s = ExceptT TypeError (StateT (Int,MetaState s) (ST s))

fresh :: Elab s MetaName
fresh = do
    (n,m) <- get
    put (n+1,m)
    pure (MetaName n)

ensureSize :: Int -> Elab s (MetaState s)
ensureSize s = do
    (n,m) <- get
    if s > V.length m then do
        m' <- V.grow m s
        V.set (V.slice (V.length m) s m') Nothing
        put (n,m')
        pure m'
    else pure m

solveMeta :: MetaName -> Val -> Elab s ()
solveMeta (MetaName n) v = do
    s <- ensureSize n
    V.write s n (Just v)

lookupMeta :: MetaName -> Elab s (Maybe Val)
lookupMeta (MetaName n) = do
    s <- ensureSize n
    V.read s n

force :: Val -> Elab s Val
force (VFlex m vs) = do
    v <- lookupMeta m
    case v of
        Nothing -> pure (VFlex m vs)
        Just v -> force =<< appMany v vs
force x = pure x

appMany :: Val -> Spine Val -> Elab s Val
appMany v ((i,x):xs) = flip appMany xs =<< appVal i v x
appMany v [] = pure v

appVal :: Icit -> Val -> Val -> Elab s Val
appVal i (VFlex m vs) v = pure (VFlex m (vs++[(i,v)]))
appVal i (VRigid n vs) v = pure (VRigid n (vs++[(i,v)]))
appVal i (VCons n vs) v = pure (VCons n (vs++[(i,v)]))
appVal i (VAbs n _ cls) v = appCls n v cls
appVal _ f x = throwError (NonFunctionApp f x)

appCls :: Name -> Val -> Cls -> Elab s Val
appCls n v (env,x) = eval (M.insert n v env) x

eval :: M.Map Name Val -> R.Raw -> Elab s Val
eval env (R.Prod n i s t) = do
    s' <- eval env s
    pure (VProd n i s' (env,t))
eval env (R.Abs n i b) = pure (VAbs n i (env,b))
eval env (R.App i f x) = do
    f' <- eval env f
    x' <- eval env x
    appVal i f' x'
eval env (R.Cons n) = pure (VCons n [])
eval env (R.Lit l) = pure (VLit l)
eval env (R.Var n) | Just v <- M.lookup n env = pure v
eval env (R.Var n) = pure (VRigid n [])
eval env R.Hole = fmap (flip VFlex []) fresh
eval env R.Type = pure VType
eval env (R.Attr r _) = eval env r