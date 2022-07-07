module ClosureConv where

import Datatypes.Lam
import Datatypes.Name
import Datatypes.Prim
import Data.Bifunctor (first,second)
import Control.Monad.State
import Control.Monad.Writer

import qualified Data.Set as S
import qualified Data.Map as M

import Data.List (partition)

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

closureConv :: S.Set Name -> CoreExpr -> CoreExpr
closureConv g (CoreLam n e) =
    let free = S.toList (S.difference (fv e) (S.union g (S.fromList n)))
    in foldr (flip CoreApp . CoreVar) (CoreLam (free ++ n) (closureConv g e)) free
closureConv g (CoreLetRec d e) =
    let
        fns = S.fromList (fmap (\(Def f _ _ _) -> f) d)
        sets = M.fromList (fmap (\(Def f n _ e) -> (,) f
            ( S.intersection fns (fv e)
            , S.difference (fv e) (S.unions [S.fromList n, fns, g]))) d)
        df = fmap S.toList (transitiveClosure sets)
    in CoreLetRec (fmap (\(Def f n t e) ->
        Def f (M.findWithDefault [] f df ++ n) t (closureConv g (addArgs df e))) d)
        (closureConv g (addArgs df e))
closureConv g (CoreApp a b) = CoreApp (closureConv g a) (closureConv g b)
closureConv g (CoreLet n x e) = CoreLet n (closureConv g x) (closureConv g e)
closureConv g (CorePrimop p x) = CorePrimop p (fmap (closureConv g) x)
closureConv g (CoreCCall f x) = CoreCCall f (fmap (closureConv g) x)
closureConv _ x = x

type Lifter = StateT CCState (Writer [Def LiftedExpr])

fresh :: Lifter Name
fresh = do
    x <- get
    put (x+1)
    pure (Gen x)

{-
type MatchMatrix a = [([Pattern],a)]

data MatchTree a
    = FlatMatch Name [(FlatPattern,MatchTree a)]
    | Rename Name Name (MatchTree a)
    | Eval a

matchesTL :: Name -> Pattern -> Bool
matchesTL n (PatternApp m _) = m == n
matchesTL _ _ = False

partitionByFirst :: MatchMatrix a -> Lifter ([(Name,[Name],MatchMatrix a)],[(Name,MatchMatrix a)])
partitionByFirst cs@((PatternApp n ns:_,_):_) = do
    let (matches, nomatch) = partition (matchesTL n . head . fst) cs
    ps <- mapM (const fresh) ns
    (other,end) <- partitionByFirst nomatch
    pure ((n,ps,matches):other,end)
partitionByFirst cs@((PatternVar _:_,_):_) = pure cs

flattenMatrix :: [Name] -> MatchMatrix a -> Lifter (MatchTree a)
flattenMatrix = undefined
-}

type MatchMatrix = [([Pattern],LiftedExpr)]

mapIndexOrInsertM :: (Monad m, Eq a) => a -> (b -> b) -> m b -> [(a,b)] -> m [(a,b)]
mapIndexOrInsertM k f c ((l,b):m) | k == l = pure ((l,f b):m)
mapIndexOrInsertM k f c (p:m) = fmap (p:) (mapIndexOrInsertM k f c m)
mapIndexOrInsertM k f c [] = fmap ((:[]) . (,) k) c

partitionCons :: MatchMatrix -> Lifter ([(Name,([Name],MatchMatrix))],MatchMatrix)
partitionCons ((PatternApp n sp:ps,b):cs) = do
    (cons,rem) <- partitionCons cs
    cons' <- mapIndexOrInsertM n (\(a,m) -> (a,(sp ++ ps,b):m)) (fmap (flip (,) [(sp ++ ps,b)]) (mapM (const fresh) sp)) cons
    pure (cons',rem)
partitionCons m = pure ([],m)

partitionVar :: MatchMatrix -> ([(Name,([Pattern],LiftedExpr))],MatchMatrix)
partitionVar ((PatternVar n:ps,b):cs) =
    let (vars,rem) = partitionVar cs
    in ((n,(ps,b)):vars,rem)
partitionVar m = ([],m)

partitionLit :: MatchMatrix -> ([(Lit,MatchMatrix)],MatchMatrix)
partitionLit cs@((PatternLit l:_,_):_) =
    let this = fmap (\(_:ps,b) -> (ps,b)) (takeWhile ((==PatternLit l) . head . fst) cs)
        other = dropWhile ((==PatternLit l) . head . fst) cs
        (lits,rem) = partitionLit other
    in ((l,this):lits,rem)
partitionLit cs = ([],cs)

compMatrix :: [LiftedExpr] -> MatchMatrix -> LiftedExpr -> Lifter LiftedExpr
compMatrix (x:xs) m@((PatternApp _ _:_,_):_) d = do
    (cons,rem) <- partitionCons m
    d' <- compMatrix (x:xs) rem d
    consRule x xs cons d'
compMatrix (x:xs) m@((PatternVar _:_,_):_) d = do
    let (vars,rem) = partitionVar m
    d' <- compMatrix (x:xs) rem d
    varRule x xs vars d'
compMatrix (x:xs) m@((PatternLit _:_,_):_) d = do
    let (lits,rem) = partitionLit m
    d' <- compMatrix (x:xs) rem d
    litRule x xs lits d'
compMatrix [] (([],e):_) d = pure e
compMatrix _ [] d = pure d
compMatrix _ _ _ = error "TYPEERROR"

litRule :: LiftedExpr -> [LiftedExpr] -> [(Lit,MatchMatrix)] -> LiftedExpr -> Lifter LiftedExpr
litRule x xs ls d = do
    ls' <- mapM (\(l,m) -> do
        e <- compMatrix xs m d
        pure (FlatPatternLit l,e)) ls
    pure (LiftedMatch x ls' d)

varRule :: LiftedExpr -> [LiftedExpr] -> [(Name,([Pattern],LiftedExpr))] -> LiftedExpr -> Lifter LiftedExpr
varRule x xs vs d =
    let m = fmap (\(n,(ps,e)) -> (ps,LiftedLet n x e)) vs
    in compMatrix xs m d

consRule :: LiftedExpr -> [LiftedExpr] -> [(Name,([Name],MatchMatrix))] -> LiftedExpr -> Lifter LiftedExpr
consRule x xs cs d = do
    cs' <- mapM (\(c,(a,m)) -> do
        e <- compMatrix (fmap LiftedVar a ++ xs) m d
        pure (FlatPatternApp c a,e)) cs
    pure (LiftedMatch x cs' d)

{-
matchesTL :: Name -> [(Pattern,a)] -> ([([Pattern],a)],[(Pattern,a)])
matchesTL n ((PatternApp m ps,a):cs) | n == m = first ((ps,a):) (matchesTL n cs)
matchesTL n ((p,a):cs) = second ((p,a):) (matchesTL n cs)
matchesTL _ [] = ([],[])

compilePatterns :: LiftedExpr -> [(Pattern,LiftedExpr)] -> Lifter LiftedExpr
compilePatterns x cs@((PatternApp n _,_):_) = do
    let (matching,not) = matchesTL n cs
  _
compilePatterns x ((PatternVar n,e):_) = pure (LiftedLet n x e)
-}

liftLams :: CoreExpr -> Lifter LiftedExpr
liftLams = cata go
    where
        go :: CoreExprF (Lifter LiftedExpr) -> Lifter LiftedExpr
        go (CoreLamF n e) = do
            f <- fresh
            e >>= tell . (:[]) . Def f n Nothing
            pure (LiftedVar f)
        go (CoreLetRecF d e) = do
            forM_ d $ \(Def f n t e) -> e >>= tell . (:[]) . Def f n t
            e
        go (CoreAppF a b) = liftM2 LiftedApp a b
        go (CoreLetF n a b) = liftM2 (LiftedLet n) a b
        go (CoreVarF n) = pure (LiftedVar n)
        go (CorePrimopF p x) = fmap (LiftedPrimop p) (sequence x)
        go (CoreLitF l) = pure (LiftedLit l)
        go (CoreCCallF f x) = fmap (LiftedCCall f) (sequence x)
        go (CoreMatchF x ps) = do
            x <- x
            m <- mapM (uncurry (fmap . ((,) . (:[])))) ps
            compMatrix [x] m (LiftedThrow "match error")

liftDefs :: [Def CoreExpr] -> Lifter ()
liftDefs = tell <=< mapM (\(Def f n t e) -> fmap (Def f n t) (liftLams e))

cconv :: S.Set Name -> CCState -> CoreExpr -> ((LiftedExpr,CCState),[Def LiftedExpr])
cconv g s = runWriter . flip runStateT s . liftLams . closureConv g

cconvDefs :: S.Set Name -> CCState -> [Def CoreExpr] -> (CCState,[Def LiftedExpr])
cconvDefs g s d = runWriter . flip execStateT s . liftDefs . fmap (\(Def f n t e) -> Def f n t (closureConv g' e)) $ d
    where
        g' = S.union g (S.fromList (fmap (\(Def f _ _ _) -> f) d))