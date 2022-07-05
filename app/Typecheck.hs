module Typecheck (runInferRec,runInfer) where

import Datatypes.Lam
import Datatypes.Name
import Datatypes.Prim

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Data.Functor.Foldable
import qualified Data.Foldable as F
import Data.Bifunctor

import qualified Data.Set as S
import qualified Data.Map as M

type InferState = (Int, Substitution)
type InferEnv = M.Map Name Polytype

type Infer = StateT InferState (ReaderT InferEnv (Except TypeError))

newvar :: Infer Name
newvar = do
    (x,s) <- get
    put (x+1,s)
    pure (Gen x)

find :: Type -> Infer Type
find t = do
    (_,s) <- get
    pure (subst s t)

instantiate :: Polytype -> Infer Type
instantiate (Forall n t) = do
    m <- sequence (M.fromSet (fmap TyVar . const newvar) n)
    find (subst m t)

env :: Infer (M.Map Name Polytype)
env = do
    g <- ask
    s <- gets snd
    pure (fmap (subst s) g)

generalize :: Type -> Infer Polytype
generalize t = do
    t' <- find t
    ev <- fmap (F.fold . fmap fv) env
    pure (Forall (S.difference (fv t') ev) t')

occurs :: Name -> Type -> Bool
occurs n t = n `S.member` fv t

bind :: Name -> Type -> Infer ()
bind n t = do
    (i,s) <- get
    put (i,M.insert n t (fmap (subst (M.singleton n t)) s))

withType :: Name -> Polytype -> Infer a -> Infer a
withType n = local . M.insert n

withTypes :: [(Name,Polytype)] -> Infer a -> Infer a
withTypes = flip (foldr (uncurry withType))

unify :: Type -> Type -> Infer ()
unify a b = do
    a <- find a
    b <- find b
    case (a,b) of
        (x,y) | x == y -> pure ()
        (TyVar n,t) | not (occurs n t) -> bind n t
        (t,TyVar n) | not (occurs n t) -> bind n t
        (TyVar n,t) -> throwError (RecursiveType n t)
        (t,TyVar n) -> throwError (RecursiveType n t)
        (App a b,App c d) -> unify a c >> unify b d
        (x,y) -> throwError (UnificationFail x y)

inferLit :: Lit -> Type
inferLit (IntLit _) = PrimTy IntTy
inferLit (StrLit _) = PrimTy StrTy

-- fix!!!
inferLetRec :: [(Name,Maybe Polytype,TypedExpr)] -> Infer [(Name,Polytype)]
inferLetRec d = do
    r <- mapM (fmap TyVar . const newvar) d
    let ft = zip (fmap (\(f,_,_)->f) d) r
    withTypes (fmap (second (Forall S.empty)) ft) $
        zipWithM_ (\(_,p,e) r -> do
            t <- infer e
            unify r t
            case p of
                Just p -> unify (rigidify p) t
                Nothing -> pure ()) d r
    zipWithM (\(_,p,_) (n,t) -> case p of
        Just p -> pure (n,p)
        Nothing -> fmap ((,) n) (generalize t)) d ft

uncurryType :: Type -> ([Type],Type)
uncurryType (App (App Arr a) b) = first (a:) (uncurryType b)
uncurryType x = ([],x)

bindPattern :: Pattern -> Type -> Infer a -> Infer a
bindPattern (PatternVar n) t f = withType n (Forall S.empty t) f
bindPattern (PatternApp c s) t f = do
    ct <- asks (M.lookup c)
    case ct of
        Just p -> do
            (at,rt) <- fmap uncurryType (instantiate p)
            foldr (uncurry bindPattern) f (zip s at)
        Nothing -> throwError (UnknownVar c)
bindPattern (PatternLit l) t f = f

infer :: TypedExpr -> Infer Type
infer (TypedLam a e) = do
    at <- fmap TyVar newvar
    rt <- withType a (Forall S.empty at) (infer e)
    find (arr at rt)
infer (TypedLetRec d e) = flip withTypes (infer e) =<< inferLetRec d
infer (TypedApp a b) = do
    bt <- infer b
    rt <- fmap TyVar newvar
    at <- infer a
    unify at (bt `arr` rt)
    find rt
infer (TypedVar n) = do
    g <- ask
    case M.lookup n g of
        Just p -> instantiate p
        Nothing -> throwError (UnknownVar n)
infer (TypedLit l) = pure (inferLit l)
infer (TypedPrimop p a) = do
    mapM_ infer a
    fmap TyVar newvar
infer (TypedCCall f a) = do
    mapM_ infer a
    fmap TyVar newvar
infer (TypedMatch x ps) = undefined

runInferRec :: InferState -> InferEnv -> [(Name,Maybe Polytype,TypedExpr)] -> Either TypeError ([(Name,Polytype)],InferState)
runInferRec s e = runExcept . flip runReaderT e . flip runStateT s . inferLetRec

runInfer :: InferState -> InferEnv -> TypedExpr -> Either TypeError (Type,InferState)
runInfer s e = runExcept . flip runReaderT e . flip runStateT s . infer