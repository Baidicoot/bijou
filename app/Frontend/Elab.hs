{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
module Frontend.Elab where

import qualified Frontend.Raw as R
import Frontend.Core
import Frontend.Common
import Univ.Name

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.ST

import qualified Data.Vector.Mutable as V
import qualified Data.Map as M

type MetaState s = V.MVector s (Maybe Val)

data TypeError = NonFunctionApp Val Val

type EvalCtx = M.Map Name Val

type Elab s = ReaderT EvalCtx (ExceptT TypeError (StateT (Int,MetaState s) (ST s)))

withDef :: Name -> Val -> Elab s a -> Elab s a
withDef n v = local (M.insert n v)

withDefs :: [(Name,Val)] -> Elab s a -> Elab s a
withDefs = local . M.union . M.fromList

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

simpl :: Val -> Elab s Val
simpl = go <=< force
    where
        go (VTop _ _ v) = go =<< force v
        go x = pure x

appMany :: Val -> Spine Val -> Elab s Val
appMany v ((i,x):xs) = flip appMany xs =<< appVal i v x
appMany v [] = pure v

appVal :: Icit -> Val -> Val -> Elab s Val
appVal i (VFlex m vs) v = pure (VFlex m (vs++[(i,v)]))
appVal i (VRigid n vs) v = pure (VRigid n (vs++[(i,v)]))
appVal i (VCons n vs) v = pure (VCons n (vs++[(i,v)]))
appVal _ (VAbs n _ cls) v = appCls n v cls
appVal i (VTop n vs f) v = fmap (VTop n (vs++[(i,v)])) (appVal i f v)
appVal _ f x = throwError (NonFunctionApp f x)

appCls :: Name -> Val -> Cls -> Elab s Val
appCls n v (env,x) = eval (M.insert n v env) x

evalType :: M.Map Name Val -> R.Raw -> Elab s VTy
evalType env r = do
    v <- eval env r
    case v of
        VAttr t u -> pure (u,t)
        v -> pure (Usage Omega Omega,v)

type family LiftedPrimop o where
    LiftedPrimop (a -> o) = PrimLit -> LiftedPrimop o
    LiftedPrimop o = PrimLit

class LiftPrim t where
    liftPrim :: t -> LiftedPrimop t

instance LiftPrim o => LiftPrim (Int -> o) where
    liftPrim f (IntLit i) = liftPrim (f i)
    liftPrim _ _ = error "typeerror"

instance LiftPrim o => LiftPrim (String -> o) where
    liftPrim f (StrLit s) = liftPrim (f s)
    liftPrim _ _ = error "typeerror"

instance LiftPrim Int where
    liftPrim i = IntLit i

instance LiftPrim String where
    liftPrim s = StrLit s

evalPrim :: Primop -> [Val] -> Elab s Val
evalPrim p = fmap (go p) . mapM simpl
    where
        go Mul [VPrimLit x, VPrimLit y] =
            VPrimLit (liftPrim @(Int -> Int -> Int) (*) x y)
        go Div [VPrimLit x,VPrimLit y] =
            VPrimLit (liftPrim @(Int -> Int -> Int) div x y)
        go Add [VPrimLit x,VPrimLit y] =
            VPrimLit (liftPrim @(Int -> Int -> Int) (+) x y)
        go Sub [VPrimLit x,VPrimLit y] =
            VPrimLit (liftPrim @(Int -> Int -> Int) (-) x y)
        go op xs = VPrimop op xs

{-
bindPat :: CorePattern -> Val -> Elab s (Maybe [(Name,Val)])
bindPat p v = go p =<< force v
    where
        go (BindVar n _) v = pure (Just [(n,v)])
        go (Uncons n ps) (VCons m sp) | n == m = do
            xs <- fmap concat . sequence <$> zipWithM (\x y -> bindPat (snd x) (snd y)) ps sp
            _

evalMatch :: M.Map Name Val -> Val -> [(CorePattern,R.Raw)] -> Elab s Val
evalMatch = undefined
-}

eval :: M.Map Name Val -> R.Raw -> Elab s Val
eval env (R.Prod n i s t) = do
    s' <- evalType env s
    pure (VProd n i s' (env,t))
eval env (R.Abs n i b) = pure (VAbs n i (env,b))
eval env (R.App i f x) = do
    f' <- eval env f
    x' <- eval env x
    appVal i f' x'
eval env (R.Cons n) = pure (VCons n [])
eval env (R.Lit l) = pure (VPrimLit l)
eval env (R.Var n) | Just v <- M.lookup n env = pure v
eval env (R.Var n) = do
    d <- ask
    case M.lookup n d of
        Just v -> pure (VTop n [] v)
        Nothing -> pure (VRigid n [])
eval env R.Hole = fmap (flip VFlex []) fresh
eval env R.Type = pure VType
eval env (R.Attr r u) = fmap (flip VAttr u) (eval env r)
eval env (R.Ann r _) = eval env r
eval env (R.Let n e b) = do
    v <- eval env e
    eval (M.insert n v env) b
eval env (R.LetRec df r) = do
    rec defs <- withDefs defs . forM df $ \(n,_,v) -> do
            fmap ((,) n) (eval env v)
    withDefs defs (eval env r)
eval env (R.Primop p r) = evalPrim p =<< mapM (eval env) r
eval env (R.Match _ _) = error "unimplemented"
--eval env (R.Match d cs) = do
    --v <- eval env d
    --evalMatch env v cs

