module Rename (renameTL,renameExpr) where

import Datatypes.Lam
import Datatypes.Name

import Data.Functor.Foldable

import Control.Monad.State
import Control.Monad.Reader

import qualified Data.Map as M

type RenameState = M.Map String Int
type Renamer = StateT RenameState (Reader RenameState)

fresh :: String -> Renamer Int
fresh s = do
    g <- get
    let i = M.findWithDefault (-1) s g + 1
    put (M.insert s i g)
    pure i

withNew :: Renamer a -> Renamer a
withNew r = do
    g <- get
    local (const g) r

renameDefs :: [(Def CoreExpr, Bool)] -> Renamer [Def CoreExpr]
renameDefs d = do
    f <- mapM (\(Def f _ _ _,b) -> case f of
                User s _ | not b -> fmap (User s) (fresh s)
                _ -> pure f) d
    withNew $
        mapM (\(n, (Def _ a t x,_)) -> do
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
            f <- mapM (\(Def n _ _ _) -> case n of
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
            g <- ask
            case M.lookup s g of
                Just i -> pure (CoreVar (User s i))
                Nothing -> pure (CoreVar n)
        go x = fmap embed (sequence x)

renameExpr :: CoreExpr -> CoreExpr
renameExpr = flip runReader mempty . flip evalStateT mempty . renameVars

renameTL :: [(Def CoreExpr,Bool)] -> [Def CoreExpr]
renameTL = flip runReader mempty . flip evalStateT mempty . renameDefs