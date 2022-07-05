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

type ANFCont = (ANFVal -> ANFExpr) -> ANFExpr

toANF :: NoPartialsExpr -> ANFifier ANFExpr
toANF = fmap (\f -> f ANFReturn) . cata go
    where
        go :: NoPartialsExprF (ANFifier ANFCont) -> ANFifier ANFCont
        go (NoPartialsAppPartialF f x) = do
            ff <- f
            xf <- x
            r <- fresh
            pure (\h -> ff (\fv -> xf (\xv -> ANFAppPartial r fv xv (h (ANFVar r)))))
        go (NoPartialsAppGlobalF f x) = do
            xf <- foldM (\xf a -> do
                af <- a
                pure (\h -> af (\av -> xf (\xv -> h (xv++[av]))))) (\h -> h []) x
            r <- fresh
            pure (\h -> xf (\xs -> ANFAppGlobal r f xs (h (ANFVar r))))
        go (NoPartialsMkPartialF f x) = do
            xf <- foldM (\xf a -> do
                af <- a
                pure (\h -> af (\av -> xf (\xv -> h (xv++[av]))))) (\h -> h []) x
            r <- fresh
            pure (\h -> xf (\xs -> ANFMkClosure r f xs (h (ANFVar r))))
        go (NoPartialsUnpackPartialF n p k) = do
            kf <- k
            pf <- p
            pure (\h -> pf (\pv -> ANFUnpackPartial n pv (kf h)))
        go (NoPartialsLetF n x k) = do
            kf <- k
            xf <- x
            pure (\h -> xf (\xv -> ANFLet n xv (kf h)))
        go (NoPartialsVarF n) = pure (\h -> h (ANFVar n))
        go (NoPartialsLabelF n) = pure (\h -> h (ANFLabel n))
        go (NoPartialsPrimopF p x) = do
            xf <- foldM (\xf a -> do
                af <- a
                pure (\h -> af (\av -> xf (\xv -> h (xv++[av]))))) (\h -> h []) x
            r <- fresh
            pure (\h -> xf (\xs -> ANFPrimop r p xs (h (ANFVar r))))
        go (NoPartialsLitF l) = pure (\h -> h (ANFLit l))
        go (NoPartialsCCallF f x) = do
            xf <- foldM (\xf a -> do
                af <- a
                pure (\h -> af (\av -> xf (\xv -> h (xv++[av]))))) (\h -> h []) x
            r <- fresh
            pure (\h -> xf (\xs -> ANFCCall r f xs (h (ANFVar r))))
        go (NoPartialsMatchF x p) =
            undefined

anfify :: ANFifyState -> NoPartialsExpr -> (ANFExpr,ANFifyState)
anfify s = flip runState s . toANF

anfifyDefs :: ANFifyState -> [Def NoPartialsExpr] -> ([Def ANFExpr],ANFifyState)
anfifyDefs s = flip runState s . mapM (\(Def n a t e) -> fmap (Def n a t) (toANF e))