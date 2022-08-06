module ClosureConv where

import Datatypes.Core
import Datatypes.Lifted
import Datatypes.Pattern
import Datatypes.Name
import Datatypes.Prim
import Data.Bifunctor (first,second)

import qualified Data.Set as S
import qualified Data.Map as M

import Data.List (partition)
import Control.Monad.State
import Control.Monad.Writer

import Data.Functor.Foldable hiding(fold)
import Data.Foldable (foldMap,fold)

type CCState = Int
type Lifter = WriterT ([LiftedDef],[(Name,Int)]) (State CCState)

addDef :: LiftedDef -> Lifter ()
addDef d = tell ([d],[])

addArity :: Name -> Int -> Lifter ()
addArity n a = tell ([],[(n,a)])

-- compute the transitive closure of a class of named sets augmented with some data d
transitiveClosure :: (Ord n, Monoid d, Eq d) => M.Map n (S.Set n,d) -> M.Map n d
transitiveClosure = iter M.empty
    where
        iter :: (Ord n, Monoid d, Eq d) => M.Map n d -> M.Map n (S.Set n,d) -> M.Map n d
        iter old sets =
            let new = fmap (\(e,d) -> d <> foldMap (snd . fold . flip M.lookup sets) e) sets
            in if old == new then new else iter new sets

appImplicit :: M.Map Name [Name] -> CoreExpr -> CoreExpr
appImplicit f = cata go
    where
        go (CoreVarF n) = case M.lookup n f of
            Just free -> foldr (flip CoreApp . CoreVar) (CoreVar n) free
            Nothing -> CoreVar n
        go x = embed x

app :: CoreExpr -> [Name] -> CoreExpr
app = foldr (flip CoreApp . CoreVar)

smash :: CoreExpr -> ([Name],CoreExpr)
smash (CoreLam n e) = let (ns,b) = smash e in (n:ns,b)
smash (CoreAnnot e t) = smash e
smash x = ([],x)

unsmash :: [Name] -> CoreExpr -> CoreExpr
unsmash (n:ns) e = CoreLam n (unsmash ns e)
unsmash [] e = e

closureConv :: S.Set Name -> CoreExpr -> CoreExpr
closureConv g l@(CoreLam _ _) =
    let (ns,e) = smash l
        free = S.toList (S.difference (fv e) (S.union (S.fromList ns) g))
    in app (unsmash (free++ns) (closureConv g e)) free
closureConv g (CoreLetRec d e) =
    let
        fns = S.fromList (fmap fst d)
        g' = S.union fns g
        sets = M.fromList (fmap (\(f,e) -> (,) f
            ( S.intersection fns (fv e)
            , S.difference (fv e) g')) d)
        df = fmap S.toList (transitiveClosure sets)
    in CoreLetRec (fmap (\(f,l) ->
        let (ns,e) = smash l
        in (f, unsmash (M.findWithDefault [] f (fmap (S.toList . snd) sets) ++ ns) (closureConv g' (appImplicit df e)))) d)
        (closureConv g (appImplicit df e))
closureConv g (CoreApp a b) = CoreApp (closureConv g a) (closureConv g b)
closureConv g (CoreLet n x e) = CoreLet n (closureConv g x) (closureConv g e)
closureConv g (CorePrimop p x) = CorePrimop p (fmap (closureConv g) x)
closureConv g (CoreCCall f x) = CoreCCall f (fmap (closureConv g) x)
closureConv g (CoreMatch e cs) = CoreMatch (closureConv g e) (fmap (second (closureConv g)) cs)
closureConv g (CoreAnnot e _) = closureConv g e
closureConv _ x = x

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

liftDef :: Name -> ([Name],CoreExpr) -> Lifter ()
liftDef n (a,e) = do
    e' <- liftLams e
    addDef (LiftedDef n a e')
    addArity n (length a)

liftLams :: CoreExpr -> Lifter LiftedExpr
liftLams e@(CoreLam _ _) = do
    f <- fresh
    liftDef f (smash e)
    pure (LiftedVar f)
liftLams (CoreLetRec d e) = do
    forM_ d $ \(f,e) -> liftDef f (smash e)
    liftLams e
liftLams (CoreMatch e cs) = do
    e' <- liftLams e
    m <- mapM (\(p,b) -> fmap ((,) [p]) (liftLams b)) cs
    compMatrix [e'] m (LiftedThrow "no match")
liftLams (CoreApp a b) = liftM2 LiftedApp (liftLams a) (liftLams b)
liftLams (CoreLet n a b) = liftM2 (LiftedLet n) (liftLams a) (liftLams b)
liftLams (CorePrimop p xs) = fmap (LiftedPrimop p) (mapM liftLams xs)
liftLams (CoreCCall f xs) = fmap (LiftedCCall f) (mapM liftLams xs)
liftLams (CoreLit l) = pure (LiftedLit l)
liftLams (CoreVar v) = pure (LiftedVar v)
liftLams (CoreCons n) = pure (LiftedCons n)
liftLams (CoreAnnot e _) = liftLams e

liftTLDefs :: [(Name,CoreExpr)] -> Lifter ()
liftTLDefs d = forM_ d $ \(f,e) -> liftDef f (smash e)

{-
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
-}
--liftDefs :: [Def CoreExpr] -> Lifter ()
--liftDefs = tell <=< mapM (\(Def f n t e) -> fmap (Def f n t) (liftLams e))

cconvDefs :: S.Set Name -> CCState -> CoreMod -> (([LiftedDef],[(Name,Int)]),CCState)
cconvDefs g s m@(CoreMod _ _ _ _ _ f) = (flip runState s . execWriterT . liftTLDefs . fmap (second (closureConv g'))) f
    where
        g' = S.union (globalsTL m) g
--cconvDefs g s d = runWriter . flip execStateT s . liftDefs . fmap (\(Def f n t e) -> Def f n t (closureConv g' e)) $ d
--    where
--        g' = S.union g (S.fromList (fmap (\(f,_) -> f) d))