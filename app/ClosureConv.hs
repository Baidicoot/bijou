module ClosureConv (cconv, cconvDefs) where

import Datatypes.Lam
import Control.Monad.State
import Control.Monad.Writer

import qualified Data.Set as S

type CCState = Int

closureConv :: LamExp -> LamExp
closureConv (Lam n e) =
    let free = S.toList (S.difference (fv e) (S.fromList n))
    in
        foldr (flip App . Var) (Lam (free ++ n) (closureConv e)) free
closureConv (App a b) = App (closureConv a) (closureConv b)
closureConv (Let n x e) = Let n (closureConv x) (closureConv e)
closureConv x = x

type Lifter = StateT CCState (Writer [DefL])

fresh :: Lifter Name
fresh = do
    x <- get
    put (x+1)
    pure (Gen x)

liftLams :: LamExp -> Lifter LiftExp
liftLams (Lam n e) = do
    name <- fresh
    e' <- liftLams e
    tell [Def name n e']
    pure (VarL name)
liftLams (Let n x e) = liftM2 (LetL n) (liftLams x) (liftLams e)
liftLams (App a b) = liftM2 AppL (liftLams a) (liftLams b)
liftLams (LetRec d e) = do
    d' <- mapM (\(Def f n e) -> fmap (Def f n) (liftLams e)) d
    tell d'
    liftLams e
liftLams (Var n) = pure (VarL n)
liftLams (Primop p) = pure (PrimopL p)

liftDefs :: [LamDef] -> Lifter ()
liftDefs = tell <=< mapM (\(Def f n e) -> fmap (Def f n) (liftLams e))

cconv :: CCState -> LamExp -> ((LiftExp,CCState),[DefL])
cconv s = runWriter . flip runStateT s . liftLams . closureConv

cconvDefs :: CCState -> [LamDef] -> (CCState,[DefL])
cconvDefs s = runWriter . fmap snd . flip runStateT s . liftDefs . fmap (\(Def f n e) -> Def f n (closureConv e))