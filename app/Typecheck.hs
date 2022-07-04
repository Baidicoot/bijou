module Typecheck (runInferDefs,runInfer) where

import Datatypes.Lam
import Datatypes.Name
import Datatypes.Prim

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Data.Functor.Foldable

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

generalize :: Type -> Infer Polytype
generalize t = do
    t' <- find t
    pure (Forall (fv t') t')

occurs :: Name -> Type -> Bool
occurs n t = n `S.member` fv t

bind :: Name -> Type -> Infer ()
bind n t = do
    (i,s) <- get
    put (i,M.insert n t (fmap (subst (M.singleton n t)) s))

withTypes :: Infer a -> [(Name,Polytype)] -> Infer a
withTypes = foldr (uncurry withType)
    where
        withType :: Name -> Polytype -> Infer a -> Infer a
        withType n = local . M.insert n

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

-- fix for annotated defs
inferDefs :: [Def CoreExpr] -> Infer [(Name,Polytype)]
inferDefs d = do
    a' <- mapM (\(Def _ a _ _) -> mapM (fmap TyVar . const newvar) a) d
    r <- mapM (fmap TyVar . const newvar) d
    let ft = zip (fmap (\(Def f _ _ _) -> f) d) (zipWith (foldr arr) r a')
    flip withTypes (fmap (\(a,b) -> (a,Forall S.empty b)) ft) $
        zipWithM_ (\(Def _ a p e) (a',r) -> do
            rt <- withTypes (infer e) (zip a (fmap (Forall S.empty) a'))
            unify r rt) d (zip a' r)
    zipWithM (\(Def _ _ p _) (n,t) -> case p of
        Just p -> unify (rigidify p) t >> pure (n,p)
        Nothing -> fmap ((,) n) (generalize t)) d ft

infer :: CoreExpr -> Infer Type
infer (CoreLam a e) = do
    a' <- mapM (fmap TyVar . const newvar) a
    t <- infer e `withTypes` zip a (fmap (Forall S.empty) a')
    find (foldr arr t a')
infer (CoreLetRec d e) = withTypes (infer e) =<< inferDefs d
infer (CoreApp a b) = do
    bt <- infer b
    rt <- fmap TyVar newvar
    at <- infer a
    unify at (bt `arr` rt)
    find rt
infer (CoreLet n x e) = do
    xt <- generalize =<< infer x
    withTypes (infer e) [(n,xt)]
infer (CoreVar n) = do
    g <- ask
    case M.lookup n g of
        Just p -> instantiate p
        Nothing -> throwError (UnknownVar n)
infer (CoreLit l) = pure (inferLit l)
infer (CorePrimop p a) = do
    mapM_ infer a
    fmap TyVar newvar
infer (CoreCCall f a) = do
    mapM_ infer a
    fmap TyVar newvar

runInferDefs :: InferState -> InferEnv -> [Def CoreExpr] -> Either TypeError ([(Name,Polytype)],InferState)
runInferDefs s e = runExcept . flip runReaderT e . flip runStateT s . inferDefs

runInfer :: InferState -> InferEnv -> CoreExpr -> Either TypeError (Type,InferState)
runInfer s e = runExcept . flip runReaderT e . flip runStateT s . infer