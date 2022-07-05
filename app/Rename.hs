module Rename (renameTL,renameExpr) where

import Datatypes.Lam
import Datatypes.Name

import Data.Functor.Foldable
import Data.Bifunctor
import Control.Monad.State
import Control.Monad.Reader

import qualified Data.Map as M
import qualified Data.Set as S

type RenameState = M.Map String Int
type Renamer = StateT RenameState (Reader (RenameState, S.Set String))

fresh :: String -> Renamer Int
fresh s = do
    g <- get
    let i = M.findWithDefault (-1) s g + 1
    put (M.insert s i g)
    pure i

withNew :: Renamer a -> Renamer a
withNew r = do
    g <- get
    local (first (const g)) r

renameDefs :: [Def CoreExpr] -> Renamer [Def CoreExpr]
renameDefs d = do
    (_,exp) <- ask
    f <- mapM (\(Def f _ _ _) -> case f of
        User s _ | s `S.member` exp -> pure (Exact s)
        User s _ -> fmap (User s) (fresh s)
        _ -> pure f) d
    withNew $
        mapM (\(n, Def _ a t x) -> do
            a <- mapM (\n -> case n of
                User s _ -> fmap (User s) (fresh s)
                _ -> pure n) a
            fmap (Def n a t) (withNew (renameVars x))) (zip f d)

renameVars :: CoreExpr -> Renamer CoreExpr
renameVars = cata go
    where
        go :: CoreExprF (Renamer CoreExpr) -> Renamer CoreExpr
        go (CoreLetF n@(User s _) x e) = do
            i <- fresh s
            liftM2 (CoreLet (User s i)) x (withNew e)
        go (CoreLetRecF d e) = do
            (_,exp) <- ask
            f <- mapM (\(Def n _ _ _) -> case n of
                User s _ | s `S.member` exp -> pure (Exact s)
                User s _ -> fmap (User s) (fresh s)
                _ -> pure n) d
            withNew $ do
                d <- mapM (\(n, Def _ a t x) -> do
                    a <- mapM (\n -> case n of
                        User s _ -> fmap (User s) (fresh s)
                        _ -> pure n) a
                    fmap (Def n a t) (withNew x)) (zip f d)
                fmap (CoreLetRec d) e
        go (CoreLamF a e) = do
            a <- mapM (\n -> case n of
                User s _ -> fmap (User s) (fresh s)
                _ -> pure n) a
            fmap (CoreLam a) (withNew e)
        go (CoreVarF n@(User s _)) = do
            (g,exp) <- ask
            case M.lookup s g of
                _ | s `S.member` exp -> pure (CoreVar (Exact s))
                Just i -> pure (CoreVar (User s i))
                Nothing -> pure (CoreVar n)
        go x = fmap embed (sequence x)

renameExpr :: S.Set String -> CoreExpr -> CoreExpr
renameExpr s = flip runReader (mempty,s) . flip evalStateT mempty . renameVars

renameTL :: S.Set String -> [Def CoreExpr] -> [Def CoreExpr]
renameTL s = flip runReader (mempty,s) . flip evalStateT mempty . renameDefs