module PartialApp (partialsMod,mkGlobalMap) where

import Datatypes.Closure
import Datatypes.Lifted
import Datatypes.Name
import Datatypes.Core

import qualified Data.Map as M
import qualified Data.Set as S

import Control.Monad.RWS

type PartialEnv = M.Map Name Int
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
    a <- ask
    pure (M.lookup n a)

registerPartial :: Name -> Int -> Name -> Partialer ()
registerPartial n sup p = modify (\(s,m) -> (s,M.insert (n,sup) p m))

makeConsFunc :: Name -> Int -> Partialer ()
makeConsFunc c a = do
    v <- replicateM a fresh
    tell [ClosureFunc c v (ClosureMkCons c (fmap ClosureVar v))]

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
                    tell [ClosureFunc p [k,extra] (ClosureUnpackPartial args (ClosureVar k) (ClosureMkPartial p' (fmap ClosureVar (args ++ [extra]))))]
                    pure p
                else do
                    tell [ClosureFunc p [k,extra] (ClosureUnpackPartial args (ClosureVar k) (ClosureAppGlobal n (fmap ClosureVar (args ++ [extra]))))]
                    pure p

makePartials :: LiftedExpr -> Partialer ClosureExpr
makePartials a@(LiftedApp _ _) = do
    x' <- mapM makePartials x
    case f of
        LiftedVar f -> mkClosure f x'
        LiftedCons f -> mkClosure f x'
        _ -> error "TYPEERROR"
    where
        (f,x) = collectArgs a
        mkClosure f x' = do
            a <- getArity f
            case a of
                Just n | n > length x -> do
                    p <- genPartial f n (length x)
                    pure (ClosureMkPartial p (reverse x'))
                Just n -> 
                    let globApp = ClosureAppGlobal f (take n (reverse x'))
                    in pure (foldr (flip ClosureAppPartial) globApp (take (length x - n) x'))
                Nothing -> pure (foldr (flip ClosureAppPartial) (ClosureVar f) x')
makePartials (LiftedLet n x e) = liftM2 (ClosureLet n) (makePartials x) (makePartials e)
makePartials (LiftedCons n) = do
    a <- getArity n
    case a of
        Just 0 -> pure (ClosureAppGlobal n [])
        Just nreq -> do
            p <- genPartial n nreq 0
            pure (ClosureMkPartial p [])
        Nothing -> error "TYPEERROR"
makePartials (LiftedVar n) = do
    a <- getArity n
    case a of
        Just nreq -> do
            p <- genPartial n nreq 0
            pure (ClosureMkPartial p [])
        Nothing -> pure (ClosureVar n)
makePartials (LiftedPrimop p x) = fmap (ClosurePrimop p) (mapM makePartials x)
makePartials (LiftedLit l) = pure (ClosureLit l)
makePartials (LiftedCCall f x) = fmap (ClosureCCall f) (mapM makePartials x)
makePartials (LiftedMatch x ps d) = do
    x <- makePartials x
    ps <- mapM (uncurry (\a -> fmap ((,) a) . makePartials)) ps
    d <- makePartials d
    pure (ClosureMatch x ps d)
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

partialsMod :: Int -> M.Map Name Int -> CoreMod -> [LiftedDef] -> (Int,[ClosureFunc])
partialsMod s g m@(CoreMod _ _ _ dd _ _) fd = (\((s,_),d) -> (s,d)) (execRWS (makeCons dd >> makePartialsDef fd) g' (s,mempty))
    where
        g' = M.unions [g,M.fromList (fmap defArity fd),aritiesTL m]