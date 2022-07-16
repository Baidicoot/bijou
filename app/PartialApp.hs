module PartialApp (partialsMod,mkGlobalMap) where

import Datatypes.Closure
import Datatypes.Lifted
import Datatypes.Name
import Datatypes.Core
import Datatypes.Pattern
import Datatypes.Prim

import qualified Data.Map as M
import qualified Data.Set as S

import Control.Monad.RWS

type PartialEnv = (M.Map Name Int, M.Map Name Repr)
type PartialState = (Int, M.Map (Name,Int) Name)
type Partialer = RWS PartialEnv [ClosureFunc] PartialState

collectArgs :: LiftedExpr -> (LiftedExpr,[LiftedExpr])
collectArgs (LiftedApp f x) = (\(f,xs) -> (f,x:xs)) (collectArgs f)
collectArgs x = (x,[])

fresh :: Partialer Name
fresh = do
    (s,m) <- get
    put (s+1,m)
    pure (Gen s)

getArity :: Name -> Partialer (Maybe Int)
getArity n = do
    (a,_) <- ask
    pure (M.lookup n a)

getRepr :: Name -> Partialer Repr
getRepr n = do
    (_,r) <- ask
    pure ((\(Just r)->r) (M.lookup n r))

registerPartial :: Name -> Int -> Name -> Partialer ()
registerPartial n sup p = modify (\(s,m) -> (s,M.insert (n,sup) p m))

makeConsFunc :: Name -> Int -> Partialer ()
makeConsFunc c a = do
    r <- getRepr c
    case r of
        Standard t -> mkStdConsFunc c t a
        Enum _ -> pure ()
        Struct -> mkStructConsFunc c a
        Newtype -> mkNewtypeConsFunc c
        Const t -> mkConstConsGlob c t

genPartial :: Name -> Int -> Int -> Partialer Name
genPartial n arity sup = do
    (_,m) <- get
    case M.lookup (n,sup) m of
        Just p -> pure p
        Nothing -> do
                p <- fresh
                k <- fresh
                extra <- fresh
                args <- mapM (const fresh) [1..sup]
                registerPartial n sup p
                if arity > sup + 1 then do
                    p' <- genPartial n arity (sup + 1)
                    tell [ClosureFunc p [k,extra] (unpackRecord (ClosureVar k) (zip args [1..])
                        (ClosureMkRecord (ClosureLabel p':fmap ClosureVar (args ++ [extra]))))]
                    pure p
                else do
                    tell [ClosureFunc p [k,extra] (unpackRecord (ClosureVar k) (zip args [1..])
                        (ClosureAppGlobal n (fmap ClosureVar (args ++ [extra]))))]
                    pure p

mkStdConsFunc :: Name -> Int -> Int -> Partialer ()
mkStdConsFunc c t a = do
    v <- replicateM a fresh
    tell [ClosureFunc c v (ClosureMkRecord (ClosureLit (IntLit t):fmap ClosureVar v))]

mkStructConsFunc :: Name -> Int -> Partialer ()
mkStructConsFunc c a = do
    v <- replicateM a fresh
    tell [ClosureFunc c v (ClosureMkRecord (fmap ClosureVar v))]

mkNewtypeConsFunc :: Name -> Partialer ()
mkNewtypeConsFunc c = do
    v <- fresh
    k <- fresh
    tell [ClosureFunc c [v,k] (ClosureVar v)]

mkConstConsGlob :: Name -> Int -> Partialer ()
mkConstConsGlob c t = do
    tell [ClosureConst c (IntLit t)]

mkCons :: Name -> [ClosureExpr] -> Partialer ClosureExpr
mkCons n xs = do
    r <- getRepr n
    a <- fmap (\(Just a)->a) (getArity n)
    case r of
        Standard i | a > length xs -> mkAppGlobal n a xs
        Standard i -> pure (ClosureMkRecord (ClosureLit (IntLit i):xs))
        Enum i -> pure (ClosureLit (IntLit i))
        Struct | a > length xs -> mkAppGlobal n a xs
        Struct -> pure (ClosureMkRecord xs)
        Const i -> pure (ClosureRef n)
        Newtype -> case xs of
            (x:_) -> pure x
            [] -> pure (ClosureMkRecord [ClosureLabel n])

appPartial :: ClosureExpr -> ClosureExpr -> Partialer ClosureExpr
appPartial f x = do
    k <- fresh
    fp <- fresh
    pure (ClosureLet k f (ClosureIndexRecord fp 0 (ClosureVar k) (ClosureAppLocal (ClosureVar fp) [ClosureVar k,x])))

appPartials :: ClosureExpr -> [ClosureExpr] -> Partialer ClosureExpr
appPartials = foldM appPartial

mkAppGlobal :: Name -> Int -> [ClosureExpr] -> Partialer ClosureExpr
mkAppGlobal f a xs | a > length xs = do
    p <- genPartial f a (length xs)
    pure (ClosureMkRecord (ClosureLabel p:xs))
mkAppGlobal f a xs =
    let appGlob = ClosureAppGlobal f (take a xs)
    in appPartials appGlob (drop a xs)

unpackRecord :: ClosureExpr -> [(Name,Int)] -> ClosureExpr -> ClosureExpr
unpackRecord r vs k = foldr (\(v,i) -> ClosureIndexRecord v i r) k vs

standardMatch :: LiftedExpr -> [(FlatPattern,LiftedExpr)] -> LiftedExpr -> Partialer ClosureExpr
standardMatch x cs d = do
    x' <- makePartials x
    r <- fresh
    cs' <- mapM (\(FlatPatternApp n vs,b) -> do
        b' <- makePartials b
        rep <- getRepr n
        case rep of
            Standard i -> pure (IntLit i,unpackRecord (ClosureVar r) (zip vs [1..]) b')
            Const i -> pure (IntLit i,b')) cs
    d' <- makePartials d
    t <- fresh
    pure (ClosureLet r x' (ClosureIndexRecord t 0 (ClosureVar r) (ClosureSwitch (ClosureVar t) cs' d')))

enumMatch :: LiftedExpr -> [(FlatPattern,LiftedExpr)] -> LiftedExpr -> Partialer ClosureExpr
enumMatch x cs d = do
    x' <- makePartials x
    cs' <- mapM (\(FlatPatternApp n _,b) -> do
        b' <- makePartials b
        rep <- getRepr n
        case rep of
            Enum i -> pure (IntLit i,b')) cs
    d' <- makePartials d
    pure (ClosureSwitch x' cs' d')

makePartials :: LiftedExpr -> Partialer ClosureExpr
makePartials e@(LiftedApp _ _) = do
    let (f,xs) = collectArgs e
    xs' <- fmap reverse (mapM makePartials xs)
    case f of
        LiftedVar f -> do
            a <- getArity f
            case a of
                Just a -> mkAppGlobal f a xs'
                Nothing -> appPartials (ClosureVar f) xs'
        LiftedCons f -> mkCons f xs'
        _ -> error "TYPEERROR"
makePartials (LiftedLet n x e) = liftM2 (ClosureLet n) (makePartials x) (makePartials e)
makePartials (LiftedCons n) = mkCons n []
makePartials (LiftedVar n) = do
    a <- getArity n
    case a of
        Just nreq -> do
            p <- genPartial n nreq 0
            pure (ClosureMkRecord [ClosureLabel p])
        Nothing -> pure (ClosureVar n)
makePartials (LiftedPrimop p x) = fmap (ClosurePrimop p) (mapM makePartials x)
makePartials (LiftedLit l) = pure (ClosureLit l)
makePartials (LiftedCCall f x) = fmap (ClosureCCall f) (mapM makePartials x)
makePartials (LiftedMatch x cs@((FlatPatternLit _,_):_) d) = do
    x' <- makePartials x
    cs' <- mapM (\(FlatPatternLit l,b) -> do
        b' <- makePartials b
        pure (l,b')) cs
    d' <- makePartials d
    pure (ClosureSwitch x' cs' d')
makePartials (LiftedMatch x ps@((FlatPatternApp n _,_):_) d) = do
    r <- getRepr n
    case r of
        Enum _ -> enumMatch x ps d
        Struct -> case ps of
            [(FlatPatternApp _ vs,b)] -> do
                x' <- makePartials x
                r <- fresh
                b' <- makePartials b
                pure (ClosureLet r x' (unpackRecord (ClosureVar r) (zip vs [0..]) b'))
            _ -> error "TYPEERROR"
        Newtype -> case ps of
            [(FlatPatternApp _ [v],b)] -> do
                x' <- makePartials x
                b' <- makePartials b
                pure (ClosureLet v x' b')
            _ -> error "TYPEERROR"
        Standard _ -> standardMatch x ps d
        Const _ -> standardMatch x ps d
makePartials (LiftedThrow e) = pure (ClosureThrow e)

makePartialsDef :: [LiftedDef] -> Partialer ()
makePartialsDef = tell <=< mapM (\(LiftedDef f n e) -> fmap (ClosureFunc f n) (makePartials e))

defArity :: LiftedDef -> (Name,Int)
defArity (LiftedDef f n _) = (f,length n)

makeCons :: [CoreADT] -> Partialer ()
makeCons = mapM_ (uncurry makeConsFunc) . M.toList . mconcat . fmap consArities

mkGlobalMap :: [LiftedDef] -> M.Map Name Int
mkGlobalMap (LiftedDef n args _:x) = M.insert n (length args) (mkGlobalMap x)
mkGlobalMap [] = mempty

partialsMod :: Int -> M.Map Name Repr -> M.Map Name Int -> CoreMod -> [LiftedDef] -> (Int,[ClosureFunc])
partialsMod s r g m@(CoreMod _ _ _ dd _ _) fd = (\((s,_),d) -> (s,d)) (execRWS (makeCons dd >> makePartialsDef fd) (g',r') (s,mempty))
    where
        g' = M.unions [g,M.fromList (fmap defArity fd),aritiesTL m]
        dr = mconcat (fmap consRepr dd)
        r' = M.union r dr