module ClosureConv where

import Datatypes.Lam
import Control.Monad.State
import Control.Monad.Writer

import qualified Data.Set as S
import qualified Data.Map as M

import Data.Functor.Foldable hiding(fold)
import Data.Foldable (foldMap,fold)

type CCState = Int

-- compute the transitive closure of a class of named sets augmented with some data d
transitiveClosure :: (Ord n, Monoid d, Eq d) => M.Map n (S.Set n,d) -> M.Map n d
transitiveClosure = iter M.empty
    where
        iter :: (Ord n, Monoid d, Eq d) => M.Map n d -> M.Map n (S.Set n,d) -> M.Map n d
        iter old sets =
            let new = fmap (\(e,d) -> d <> foldMap (snd . fold . flip M.lookup sets) e) sets
            in if old == new then new else iter new sets

addArgs :: M.Map Name [Name] -> CoreExpr -> CoreExpr
addArgs f = cata go
    where
        go (CoreVarF n) = case M.lookup n f of
            Just free -> foldr (flip CoreApp . CoreVar) (CoreVar n) free
            Nothing -> CoreVar n
        go x = embed x

closureConv :: CoreExpr -> CoreExpr
closureConv (CoreLam n e) =
    let free = S.toList (S.difference (fv e) (S.fromList n))
    in foldr (flip CoreApp . CoreVar) (CoreLam (free ++ n) (closureConv e)) free
-- todo: do this part
closureConv (CoreLetRec d e) =
    let
        fns = S.fromList (fmap (\(Def f _ _) -> f) d)
        sets = M.fromList (fmap (\(Def f n e) -> (,) f
            ( S.intersection fns (fv e)
            , S.difference (fv e) (S.union (S.fromList n) fns))) d)
        df = fmap S.toList (transitiveClosure sets)
    in CoreLetRec (fmap (\(Def f n e) ->
        Def f (M.findWithDefault [] f df ++ n) (closureConv (addArgs df e))) d)
        (closureConv (addArgs df e))
closureConv (CoreApp a b) = CoreApp (closureConv a) (closureConv b)
closureConv (CoreLet n x e) = CoreLet n (closureConv x) (closureConv e)
closureConv (CorePrimop p x) = CorePrimop p (fmap closureConv x)
closureConv (CoreCCall f x) = CoreCCall f (fmap closureConv x)
closureConv x = x

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
        go (CorePrimopF p x) = fmap (LiftedPrimop p) (sequence x)
        go (CoreLitF l) = pure (LiftedLit l)
        go (CoreCCallF f x) = fmap (LiftedCCall f) (sequence x)

liftDefs :: [Def CoreExpr] -> Lifter ()
liftDefs = tell <=< mapM (\(Def f n e) -> fmap (Def f n) (liftLams e))

cconv :: CCState -> CoreExpr -> ((LiftedExpr,CCState),[Def LiftedExpr])
cconv s = runWriter . flip runStateT s . liftLams . closureConv

cconvDefs :: CCState -> [Def CoreExpr] -> (CCState,[Def LiftedExpr])
cconvDefs s = runWriter . flip execStateT s . liftDefs . fmap (\(Def f n e) -> Def f n (closureConv e))