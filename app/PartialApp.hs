{-# LANGUAGE OverloadedStrings #-}
--module PartialApp (partials, partialsDef, mkGlobalMap) where
module PartialApp where

import Datatypes.Lam
import Datatypes.Name

import qualified Data.Map as M
import qualified Data.Set as S

import Control.Monad.Writer
import Control.Monad.State

type PartialState = (Int, M.Map (Name,Int) Name)
type Partialer = StateT PartialState (Writer [Def NoPartialsExpr])

collectArgs :: LiftedExpr -> (LiftedExpr,[LiftedExpr])
collectArgs (LiftedApp f x) = (\(f,xs) -> (f,x:xs)) (collectArgs f)
collectArgs x = (x,[])

fresh :: Partialer Name
fresh = do
    (s,m) <- get
    put (s+1,m)
    pure (Gen s)

registerPartial :: Name -> Int -> Name -> Partialer ()
registerPartial n nargs p = modify (\(s,m) -> (s,M.insert (n,nargs) p m))

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
                    tell [Def p [k,extra] Nothing (NoPartialsUnpackPartial args (NoPartialsVar k) (NoPartialsMkPartial p' (fmap NoPartialsVar (args ++ [extra]))))]
                    pure p
                else do
                    tell [Def p [k,extra] Nothing (NoPartialsUnpackPartial args (NoPartialsVar k) (NoPartialsAppGlobal n (fmap NoPartialsVar (args ++ [extra]))))]
                    pure p

makePartials :: M.Map Name Int -> LiftedExpr -> Partialer NoPartialsExpr
makePartials g a@(LiftedApp _ _) = do
    x' <- mapM (makePartials g) x
    case f of
        LiftedVar f -> case M.lookup f g of
            Just n | n > length x -> do
                p <- genPartial f (n - length x) (length x)
                pure (NoPartialsMkPartial p (reverse x'))
            Just n -> 
                let globApp = NoPartialsAppGlobal f (take n (reverse x'))
                in pure (foldr (flip NoPartialsAppPartial) globApp (take (length x - n) x'))
            Nothing -> pure (foldr (flip NoPartialsAppPartial) (NoPartialsVar f) x')
        _ -> error "TYPEERROR"
    where
        (f,x) = collectArgs a
makePartials g (LiftedLet n x e) = liftM2 (NoPartialsLet n) (makePartials g x) (makePartials g e)
makePartials g (LiftedVar n) = case M.lookup n g of
    Just nreq -> do
        p <- genPartial n nreq 0
        pure (NoPartialsMkPartial p [])
    Nothing -> pure (NoPartialsVar n)
makePartials g (LiftedPrimop p x) = fmap (NoPartialsPrimop p) (mapM (makePartials g) x)
makePartials g (LiftedLit l) = pure (NoPartialsLit l)
makePartials g (LiftedCCall f x) = fmap (NoPartialsCCall f) (mapM (makePartials g) x)

makePartialsDef :: M.Map Name Int -> [Def LiftedExpr] -> Partialer ()
makePartialsDef g = tell <=< mapM (\(Def f n t e) -> fmap (Def f n Nothing) (makePartials g e))

mkGlobalMap :: [Def LiftedExpr] -> M.Map Name Int
mkGlobalMap (Def n args _ _:x) = M.insert n (length args) (mkGlobalMap x)
mkGlobalMap [] = mempty

partials :: M.Map Name Int -> PartialState -> LiftedExpr -> ((NoPartialsExpr,PartialState),[Def NoPartialsExpr])
partials g s = runWriter . flip runStateT s . makePartials g

partialsDef :: M.Map Name Int -> PartialState -> [Def LiftedExpr] -> (PartialState,[Def NoPartialsExpr])
partialsDef g s = runWriter . flip execStateT s . makePartialsDef g