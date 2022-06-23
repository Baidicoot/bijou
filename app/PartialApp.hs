module PartialApp where

import Datatypes.Lam

import qualified Data.Map as M
import qualified Data.Set as S

import Control.Monad.Writer
import Control.Monad.State

type PartialState = (Int, M.Map (Name,Int) Name)
type Partialer = StateT PartialState (Writer [DefP])

collectArgs :: LiftExp -> (LiftExp,[LiftExp])
collectArgs (AppL f x) = (\(f,xs) -> (f,x:xs)) (collectArgs f)
collectArgs x = (x,[])

fresh :: Partialer Name
fresh = do
    (s,m) <- get
    put (s+1,m)
    pure (Gen s)

registerPartial :: Name -> Int -> Name -> Partialer ()
registerPartial n nargs p = modify (\(s,m) -> (s,M.insert (n,nargs) p m))

-- partials map uses no. args remaining
genPartial :: Name -> Int -> Int -> Partialer Name
genPartial n nrem nsup = do
    (_,m) <- get
    case M.lookup (n,nrem) m of
        Just p -> pure p
        Nothing -> do
                p <- fresh
                k <- fresh
                extra <- fresh
                args <- mapM (const fresh) [1..nsup]
                registerPartial n nrem p
                if nrem > 1 then do
                    p' <- genPartial n (nrem - 1) (nsup + 1)
                    tell [Def p [k,extra] (UnpackP args (VarP k) (MkPartialP p' (fmap VarP (args ++ [extra]))))]
                    pure p
                else do
                    tell [Def p [k,extra] (UnpackP args (VarP k) (AppGlobalP n (fmap VarP (args ++ [extra]))))]
                    pure p

makePartials :: M.Map Name Int -> LiftExp -> Partialer PartialExp
makePartials g a@(AppL _ _) = do
    x' <- mapM (makePartials g) x
    case f of
        VarL f -> case M.lookup f g of
            Just n | n < length x -> do
                p <- genPartial f (n - length x) (length x)
                pure (MkPartialP p (reverse x'))
            Just n -> pure (AppGlobalP f (reverse x'))
            Nothing -> pure (foldr (flip AppPartialP) (VarP f) x')
        _ -> error "TYPEERROR"
    where
        (f,x) = collectArgs a
makePartials g (LetL n x e) = liftM2 (LetP n) (makePartials g x) (makePartials g e)
makePartials g (VarL n) = case M.lookup n g of
    Just nreq -> do
        p <- genPartial n nreq 0
        pure (MkPartialP p [])
    Nothing -> pure (VarP n)
makePartials g (PrimopL p) = pure (PrimopP p)

partials :: M.Map Name Int -> PartialState -> LiftExp -> ((PartialExp,PartialState),[DefP])
partials g s = runWriter . flip runStateT s . makePartials g