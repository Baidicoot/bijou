module ANFify where

import Datatypes.Lam
import Datatypes.Name
import Data.Functor.Foldable

import Control.Monad.State

import Control.Arrow

type ANFifyState = Int
type ANFifier = State Int

fresh :: ANFifier Name
fresh = do
    x <- get
    put (x+1)
    pure (Gen x)

toANF :: NoPartialsExpr -> ANFifier ANFExpr
toANF = fmap (\(f,v) -> f (ANFReturn v)) . cata go
    where
        go (NoPartialsAppPartialF f x) = do
            (ff,fv) <- f
            (xf,xv) <- x
            r <- fresh
            pure (ff . xf . ANFAppPartial r fv xv, ANFVar r)
        go (NoPartialsAppGlobalF f x) = do
            (xf,xs) <- foldM (\(xf,vs) a -> do
                    (af,av) <- a
                    pure (xf . af,vs++[av])) (id,[]) x
            r <- fresh
            pure (xf . ANFAppGlobal r f xs, ANFVar r)
        go (NoPartialsMkPartialF f x) = do
            (xf,xs) <- foldM (\(xf,vs) a -> do
                    (af,av) <- a
                    pure (xf . af,vs++[av])) (id,[]) x
            r <- fresh
            pure (xf . ANFMkClosure r f xs, ANFVar r)
        go (NoPartialsUnpackPartialF n p k) = do
            (kf,kv) <- k
            (pf,pv) <- p
            pure (pf . ANFUnpackPartial n pv . kf, kv)
        go (NoPartialsLetF n x k) = do
            (kf,kv) <- k
            (xf,xv) <- x
            pure (xf . ANFLet n xv . kf, kv)
        go (NoPartialsVarF n) = pure (id, ANFVar n)
        go (NoPartialsLabelF n) = pure (id, ANFLabel n)
        go (NoPartialsPrimopF p x) = do
            (xf,xs) <- foldM (\(xf,vs) a -> do
                    (af,av) <- a
                    pure (xf . af,vs++[av])) (id,[]) x
            r <- fresh
            pure (xf . ANFPrimop r p xs, ANFVar r)
        go (NoPartialsLitF l) = pure (id, ANFLit l)
        go (NoPartialsCCallF f x) = do
            (xf,xs) <- foldM (\(xf,vs) a -> do
                    (af,av) <- a
                    pure (xf . af,vs++[av])) (id,[]) x
            r <- fresh
            pure (xf . ANFCCall r f xs, ANFVar r)

anfify :: ANFifyState -> NoPartialsExpr -> (ANFExpr,ANFifyState)
anfify s = flip runState s . toANF

anfifyDefs :: ANFifyState -> [Def NoPartialsExpr] -> ([Def ANFExpr],ANFifyState)
anfifyDefs s = flip runState s . mapM (\(Def n a t e) -> fmap (Def n a t) (toANF e))