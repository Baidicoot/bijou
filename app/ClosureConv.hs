module ClosureConv where

import Datatypes.Lam
import Control.Monad.State
import Control.Monad.Writer

import qualified Data.Set as S

import Data.Functor.Foldable

type CCState = Int

{-
closureConv :: CoreExpr -> CoreExpr
closureConv (Lam n e) =
    let free = S.toList (S.difference (fv e) (S.fromList n))
    in
        foldr (flip App . Var) (Lam (free ++ n) (closureConv e)) free
closureConv (App a b) = App (closureConv a) (closureConv b)
closureConv (Let n x e) = Let n (closureConv x) (closureConv e)
closureConv x = x
-}

closureConv :: CoreExpr -> CoreExpr
closureConv = cata go
    where
        go :: CoreExprF CoreExpr -> CoreExpr
        go (CoreLamF n e) =
            let free = S.toList (S.difference (fv e) (S.fromList n))
            in foldr (flip CoreApp . CoreVar) (CoreLam (free ++ n) e) free
        go x = embed x

type Lifter = StateT CCState (Writer [Def LiftedExpr])

fresh :: Lifter Name
fresh = do
    x <- get
    put (x+1)
    pure (Gen x)

liftLams :: CoreExpr -> Lifter LiftedExpr
liftLams = cata go
    where
        go :: CoreExprF (Lifter LiftedExpr) -> Lifter LiftedExpr
        go (CoreLamF n e) = do
            f <- fresh
            e >>= tell . (:[]) . Def f n
            pure (LiftedVar f)
        go (CoreLetRecF d e) = do
            forM_ d $ \(Def f n e) -> e >>= tell . (:[]) . Def f n
            e
        go (CoreAppF a b) = liftM2 LiftedApp a b
        go (CoreLetF n a b) = liftM2 (LiftedLet n) a b
        go (CoreVarF n) = pure (LiftedVar n)
        go (CorePrimopF p) = pure (LiftedPrimop p)


liftDefs :: [Def CoreExpr] -> Lifter ()
liftDefs = tell <=< mapM (\(Def f n e) -> fmap (Def f n) (liftLams e))

cconv :: CCState -> CoreExpr -> ((LiftedExpr,CCState),[Def LiftedExpr])
cconv s = runWriter . flip runStateT s . liftLams . closureConv

cconvDefs :: CCState -> [Def CoreExpr] -> (CCState,[Def LiftedExpr])
cconvDefs s = runWriter . flip execStateT s . liftDefs . fmap (\(Def f n e) -> Def f n (closureConv e))