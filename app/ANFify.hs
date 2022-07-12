module ANFify where

import Datatypes.Pattern
import Datatypes.ANF
import Datatypes.Closure
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

type ANFCont = (ANFVal -> ANFExpr) -> ANFExpr

toANF :: ClosureExpr -> ANFifier ANFExpr
toANF = fmap (\f -> f ANFReturn) . cata go
    where
        go :: ClosureExprF (ANFifier ANFCont) -> ANFifier ANFCont
        go (ClosureAppPartialF f x) = do
            ff <- f
            xf <- x
            r <- fresh
            pure (\h -> ff (\fv -> xf (\xv -> ANFAppPartial r fv xv (h (ANFVar r)))))
        go (ClosureAppGlobalF f x) = do
            xf <- foldM (\xf a -> do
                af <- a
                pure (\h -> af (\av -> xf (\xv -> h (xv++[av]))))) (\h -> h []) x
            r <- fresh
            pure (\h -> xf (\xs -> ANFAppGlobal r f xs (h (ANFVar r))))
        go (ClosureMkPartialF f x) = do
            xf <- foldM (\xf a -> do
                af <- a
                pure (\h -> af (\av -> xf (\xv -> h (xv++[av]))))) (\h -> h []) x
            r <- fresh
            pure (\h -> xf (\xs -> ANFMkClosure r f xs (h (ANFVar r))))
        go (ClosureUnpackPartialF n p k) = do
            kf <- k
            pf <- p
            pure (\h -> pf (\pv -> ANFUnpackPartial n pv (kf h)))
        go (ClosureLetF n x k) = do
            kf <- k
            xf <- x
            pure (\h -> xf (\xv -> ANFLet n xv (kf h)))
        go (ClosureVarF n) = pure (\h -> h (ANFVar n))
        go (ClosureLabelF n) = pure (\h -> h (ANFLabel n))
        go (ClosurePrimopF p x) = do
            xf <- foldM (\xf a -> do
                af <- a
                pure (\h -> af (\av -> xf (\xv -> h (xv++[av]))))) (\h -> h []) x
            r <- fresh
            pure (\h -> xf (\xs -> ANFPrimop r p xs (h (ANFVar r))))
        go (ClosureLitF l) = pure (\h -> h (ANFLit l))
        go (ClosureCCallF f x) = do
            xf <- foldM (\xf a -> do
                af <- a
                pure (\h -> af (\av -> xf (\xv -> h (xv++[av]))))) (\h -> h []) x
            r <- fresh
            pure (\h -> xf (\xs -> ANFCCall r f xs (h (ANFVar r))))
        go (ClosureMatchF x p d) = do
            xf <- x
            df <- d
            pf <- mapM (uncurry (fmap . (,))) p
            pure (\h -> xf (\xv -> ANFMatch xv (fmap (\(a,b) -> (a,b h)) pf) (df h)))
        go (ClosureMkConsF c x) = do
            xf <- foldM (\xf a -> do
                af <- a
                pure (\h -> af (\av -> xf (\xv -> h (xv++[av]))))) (\h -> h []) x
            r <- fresh
            pure (\h -> xf (\xs -> ANFMkCons r c xs (h (ANFVar r))))
        go (ClosureThrowF e) = pure (\h -> h (ANFThrow e))

anfifyDefs :: ANFifyState -> [ClosureFunc] -> ([ANFFunc],ANFifyState)
anfifyDefs s = flip runState s . mapM (\(ClosureFunc n a e) -> fmap (ANFFunc n a) (toANF e))