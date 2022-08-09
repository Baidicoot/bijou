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

import Control.Monad.State.Lazy
import Control.Monad.Except
import Control.Monad.ST.Lazy

import qualified Data.Vector.Mutable as V
import qualified Data.Map as M

fresh :: ElabNoExcept s MetaName
fresh = do
    (n,m) <- get
    put (n+1,m)
    pure (MetaName n)

freshVar :: ElabNoExcept s Name
freshVar = do
    (n,m) <- get
    put (n+1,m)
    pure (Gen n)

ensureSize :: Int -> ElabNoExcept s (MetaState s)
ensureSize s = do
    (n,m) <- get
    if s > V.length m then do
        m' <- V.grow m s
        V.set (V.slice (V.length m) s m') Nothing
        put (n,m')
        pure m'
    else pure m

solveMeta :: MetaName -> (Val s) -> ElabNoExcept s ()
solveMeta (MetaName n) v = do
    s <- ensureSize n
    V.write s n (Just v)

lookupMeta :: MetaName -> ElabNoExcept s (Maybe (Val s))
lookupMeta (MetaName n) = do
    s <- ensureSize n
    V.read s n

force :: (Val s) -> ElabNoExcept s (Val s)
force (VFlex m vs) = do
    v <- lookupMeta m
    case v of
        Nothing -> pure (VFlex m vs)
        Just v -> force =<< appMany v vs
force x = pure x

simpl :: (Val s) -> ElabNoExcept s (Val s)
simpl = go <=< force
    where
        go (VTop _ _ v) = go =<< force v
        go x = pure x

appMany :: (Val s) -> Spine (Val s) -> ElabNoExcept s (Val s)
appMany v ((i,x):xs) = flip appMany xs =<< appVal i v x
appMany v [] = pure v

appVal :: Icit -> Val s -> Val s -> ElabNoExcept s (Val s)
appVal i (VFlex m vs) v = pure (VFlex m (vs++[(i,v)]))
appVal i (VRigid n vs) v = pure (VRigid n (vs++[(i,v)]))
appVal i (VCons n vs) v = pure (VCons n (vs++[(i,v)]))
appVal _ (VAbs _ _ cls) v = appCls cls v
appVal i (VTop n vs f) v = fmap (VTop n (vs++[(i,v)])) (appVal i f v)
appVal _ f x = error "unreachable"

evalType :: EvalCtx s -> R.Raw -> ElabNoExcept s (VTy s)
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

evalPrim :: Primop -> [(Val s)] -> ElabNoExcept s (Val s)
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
bindPat :: CorePattern -> (Val s) -> Elab s (Maybe [(Name,(Val s))])
bindPat p v = go p =<< force v
    where
        go (BindVar n _) v = pure (Just [(n,v)])
        go (Uncons n ps) (VCons m sp) | n == m = do
            xs <- fmap concat . sequence <$> zipWithM (\x y -> bindPat (snd x) (snd y)) ps sp
            _

evalMatch :: M.Map Name (Val s) -> (Val s) -> [(CorePattern,R.Raw)] -> Elab s (Val s)
evalMatch = undefined
-}

{-
unfoldDefsSp :: [Name] -> Spine (Val s) -> Spine (Val s)
unfoldDefsSp ns = fmap (second (unfoldDefs ns))

unfoldDefsCls :: [Name] -> Cls s -> Cls s
unfoldDefsCls ns (Cls f) = Cls (fmap (fmap (unfoldDefs ns)) f)

-- fully unfold top-level definitions ns
unfoldDefs :: [Name] -> (Val s) -> (Val s)
unfoldDefs ns (VTop n _ v) | elem n ns = unfoldDefs ns v
unfoldDefs ns (VTop n sp v) = VTop n (unfoldDefsSp ns sp) (unfoldDefs ns v)
unfoldDefs ns (VFlex m sp) = VFlex m (unfoldDefsSp ns sp)
unfoldDefs ns (VRigid n sp) = VRigid n (unfoldDefsSp ns sp)
unfoldDefs ns (VCons n sp) = VCons n (unfoldDefsSp ns sp)
unfoldDefs ns (VAbs n i cls) = VAbs n i (unfoldDefsCls ns cls)
unfoldDefs ns (VAttr v u) = VAttr (unfoldDefs ns v) u
unfoldDefs ns (VProd n i (u,t) cls) = VProd n i (u,unfoldDefs ns t) (unfoldDefsCls ns cls)
unfoldDefs ns (VPrimop p vs) = VPrimop p (fmap (unfoldDefs ns) vs)
unfoldDefs _ x = x
-}

substVal :: Name -> Val s -> Val s -> Val s
substVal n v x = case x of

-- probably this should produce 'ElabNoExcept s (Val s)' o.e.
eval :: EvalCtx s -> R.Raw -> ElabNoExcept s (Val s)
eval env (R.Prod n i s t) = do
    s' <- evalType env s
    pure (VProd n i s' (Cls (\v -> eval (addVar n v env) t)))
eval env (R.Abs n i b) = pure (VAbs n i (Cls (\v -> eval (addVar n v env) b)))
eval env (R.App i f x) = do
    f' <- eval env f
    x' <- eval env x
    appVal i f' x'
eval env (R.Cons n) = pure (VCons n [])
eval env (R.Lit l) = pure (VPrimLit l)
eval env (R.Var n) | Just v <- M.lookup n (varDefs env) = pure v
eval env (R.Var n) | Just v <- M.lookup n (recDefs env) = pure (VTop n [] v)
eval env (R.Var n) = error "unreachable"
eval env R.Hole = fmap (flip VFlex []) fresh
eval env R.Type = pure VType
eval env (R.Attr r u) = fmap (flip VAttr u) (eval env r)
eval env (R.Ann r _) = eval env r
eval env (R.Let n e b) = do
    v <- eval env e
    eval (addVar n v env) b
eval env (R.LetRec df r) = do
    rec defs <- forM df $ \(n,_,v) -> do
            fmap ((,) n) (eval (addRecs defs env) v)
    eval (addRecs defs env) r
eval env (R.Primop p r) = evalPrim p =<< mapM (eval env) r
eval env (R.Match _ _) = error "unimplemented"
--eval env (R.Match d cs) = do
    --v <- eval env d
    --evalMatch env v cs

-- given a spine of free variables that can appear in the rhs, construct an inverse substitution
invert :: Spine (Val s) -> Elab s [(Name,(Icit,Name))]
invert ((i,x):xs) = do
    v <- lift (force x)
    case v of
        VRigid n [] -> do
            f <- lift freshVar
            fmap ((n,(i,f)):) (invert xs)
        v -> throwError (NonVarSpine v)
invert [] = pure []

-- apply the inverse substitution, and do the occurs check
substInv :: MetaName -> [(Name,(Icit,Name))] -> Val s -> Elab s (Val s)
substInv m s = go
    where
        go :: Val s -> Elab s (Val s)
        go = goF <=< lift . force

        goF :: Val s -> Elab s (Val s)
        goF (VFlex m' _) | m == m' = throwError (Occurs m)
        goF (VFlex m' sp) = goSp (VFlex m' []) sp
        goF (VRigid n sp) | Just (i,n') <- lookup n s = goSp (VRigid n' []) sp
        goF v@(VRigid _ _) = throwError (CantInvert v)
        -- not sure if evaluating here can be bypassed
        goF (VTop _ _ v) = go v
        goF (VCons n sp) = goSp (VCons n []) sp
        goF (VAbs n i c) = do
            -- I don't think we need to do renaming here?
            b <- go =<< lift (appCls c (VRigid n []))
            pure (VAbs n i (Cls (\v -> pure (substVal n v b))))
        goF (VProd n i (u,t) c) = do
            t' <- go t
            s <- go =<< lift (appCls c (VRigid n []))
            pure (VProd n i (u,t') (Cls (\v -> pure (substVal n v s))))
        goF (VAttr v u) = fmap (flip VAttr u) (go v)
        goF (VPrimop p vs) = fmap (VPrimop p) (mapM go vs)
        goF x = pure x

        goSp :: Val s -> Spine (Val s) -> Elab s (Val s)
        goSp = _