module Typecheck (runInferMod) where

import Datatypes.Type
import Datatypes.Core
import Datatypes.Name
import Datatypes.Prim
import Datatypes.Pattern

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer
import Data.Functor.Foldable
import qualified Data.Foldable as F
import Data.Bifunctor

import qualified Data.Set as S
import qualified Data.Map as M

type InferState = (Int, Substitution)
type InferEnv = M.Map Name Polytype

type Infer = StateT InferState (ReaderT InferEnv (ExceptT TypeError (Writer [Debug])))

type Debug = String

debug :: Debug -> Infer ()
debug s = tell [s]

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
    t <- find (subst m t)
    pure t

env :: Infer (M.Map Name Polytype)
env = do
    g <- ask
    s <- gets snd
    pure (fmap (substPoly s) g)

generalize :: Type -> Infer Polytype
generalize t = do
    ev <- fmap (F.fold . fmap fv) env
    (Forall rv t') <- unrigidify =<< find t
    pure (Forall (S.difference (S.union (fv t') rv) ev) t')

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
    debug (show (a,b))
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

rigidify :: Polytype -> Infer Type
rigidify (Forall v t) = do
    ts <- mapM (\t->fmap ((,) t . Rigid) newvar) (S.toList v)
    pure (subst (M.fromList ts) t)

unrigidify :: Type -> Infer Polytype
unrigidify = fmap (uncurry Forall) . cata go
    where
        go :: TypeF (Infer (S.Set Name,Type)) -> Infer (S.Set Name,Type)
        go (RigidF n) = pure (S.singleton n,TyVar n)
        go x = do
            x' <- sequence x
            pure (F.fold (fmap fst x'),embed (fmap snd x'))

-- fix mutually recursive functions
{-
subtypePoly :: Polytype -> Polytype -> Infer ()
subtypePoly p@(Forall ps _) q@(Forall qs _) = do
    t <- instantiate p
    u <- instantiate q
    unify t u
    ps' <- mapM (find . TyVar) (S.toAscList ps)
    qs' <- mapM (find . TyVar) (S.toAscList qs)
    if ps' /= qs' then
        throwError (UnificationPoly p q)
    else
        pure ()
-}

generalizesTo :: Type -> Polytype -> Infer ()
generalizesTo t = unify t <=< rigidify

getAnnot :: CoreExpr -> Maybe Polytype
getAnnot (CoreAnnot _ p) = Just p
getAnnot _ = Nothing

inferLit :: Lit -> Type
inferLit (IntLit _) = PrimTy IntTy
inferLit (StrLit _) = PrimTy StrTy

inferLetRec :: [(Name,CoreExpr)] -> Infer [(Name,Polytype)]
inferLetRec d = do
    mono <- forM d $ \(f,e) ->
        case getAnnot e of
            Just p -> pure ((f,e),p)
            Nothing -> pure . (,) (f,e) . Forall S.empty . TyVar =<< newvar
    ts <- withTypes (fmap (first fst) mono) . forM mono $ \((f,e),p) -> do
        t <- infer e
        generalizesTo t p
        pure (f,t)
    mapM (\(f,t) -> fmap ((,) f) (generalize t)) ts

bindPattern :: Pattern -> Type -> Infer [(Name,Polytype)]
bindPattern (PatternVar n) t = pure [(n,Forall S.empty t)]
bindPattern (PatternApp c s) t = do
    ct <- asks (M.lookup c)
    case ct of
        Just p -> do
            (at,rt) <- fmap unarrs (instantiate p)
            unify t rt
            fmap concat (zipWithM bindPattern s at)
bindPattern (PatternLit l) t = do
    unify t (inferLit l)
    pure []

lookupName :: Name -> Infer Polytype
lookupName n = do
    g <- ask
    case M.lookup n g of
        Just p -> pure p
        Nothing -> throwError (UnknownVar n)

infer :: CoreExpr -> Infer Type
infer (CoreAnnot e p) = do
    t <- infer e
    generalizesTo t p
    instantiate p
infer (CoreLam a e) = do
    at <- fmap TyVar newvar
    rt <- withType a (Forall S.empty at) (infer e)
    find (arr at rt)
infer (CoreLet n d e) = do
    p <- generalize =<< infer d
    withType n p (infer e)
infer (CoreLetRec d e) = do
    fs <- inferLetRec d
    debug (show fs)
    flip withTypes (infer e) fs
infer (CoreApp a b) = do
    bt <- infer b
    rt <- fmap TyVar newvar
    at <- infer a
    unify at (bt `arr` rt)
    find rt
infer (CoreVar n) = instantiate =<< lookupName n
infer (CoreLit l) = pure (inferLit l)
infer (CorePrimop p a) = do
    mapM_ infer a
    fmap TyVar newvar
infer (CoreCCall f a) = do
    mapM_ infer a
    fmap TyVar newvar
infer (CoreMatch x ps) = do
    t <- infer x
    r <- fmap TyVar newvar
    mapM_ (\(p,e) -> do
        vs <- bindPattern p t
        rt <- withTypes vs (infer e)
        unify r rt) ps
    find r
infer (CoreCons n) = instantiate =<< lookupName n

runInferMod :: Int -> M.Map Name Polytype -> CoreMod -> (Either TypeError ([(Name,Polytype)],Int),[Debug])
runInferMod s g (CoreMod _ _ d _ f) =
    first (fmap (second fst)) (runWriter (runExceptT (runReaderT (runStateT (inferLetRec f) (s,mempty)) g')))
    where
        g' = M.unions (g:fmap consTypes d)