{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Desugar (desugarMod) where

import Datatypes.AST
import Datatypes.Core
import Datatypes.Name
import Datatypes.Pattern
import Datatypes.Type

import Data.Functor.Foldable
import Data.Bifunctor
import Data.Either (partitionEithers)
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

import qualified Data.Map as M
import qualified Data.Set as S

-- counter for each variable
type RenameState = M.Map Ident Int
-- exports, imports + locally bound names, constructors
type RenameEnv = (M.Map Ident Name,S.Set Name)

data NameError
    = UnknownIdent Ident
    | InvalidPattern ASTPattern
    | MissingDefs [Name]
    | UntypedExtern Name
    | MultipleEntry [Ident]

type Renamer = StateT RenameState (ReaderT RenameEnv (Except NameError))

identToStr :: Ident -> String
identToStr (Unqualified s) = s
identToStr (Qualified _ s) = s
identToStr Hole = "_"

fresh :: Ident -> Renamer Name
fresh s = do
    g <- get
    let i = M.findWithDefault 0 s g
    put (M.insert s (i+1) g)
    pure (User (identToStr s) i)

lookupIdent :: Ident -> Renamer Name
lookupIdent s = do
    (g,_) <- ask
    case M.lookup s g of
        Just n -> pure n
        Nothing -> throwError (UnknownIdent s)

isCons :: Name -> Renamer Bool
isCons n = do
    (_,c) <- ask
    pure (S.member n c)

withVars :: [(Ident,Name)] -> Renamer a -> Renamer a
withVars m = local (\(i,c) -> (M.union (M.fromList m) i,c))

bound :: Renamer (S.Set Name)
bound = do
    (g,_) <- ask
    pure (S.fromList (M.elems g))

bind :: Ident -> Renamer a -> Renamer (Name,a)
bind i f = do
    tmp <- bindMany [i] f
    let ([n],a) = tmp
    pure (n,a)

bindMany :: [Ident] -> Renamer a -> Renamer ([Name],a)
bindMany is f = do
    b <- mapM (\i -> fmap ((,) i) (fresh i)) is
    fmap ((,) (fmap snd b)) (withVars b f)

bindExact :: [Ident] -> Renamer a -> Renamer ([Name],a)
bindExact is =
    let is' = fmap (Exact . identToStr) is
    in fmap ((,) is') . (withVars (zip is is'))

patternBinds :: ASTPattern -> Renamer a -> Renamer (Pattern,a)
patternBinds (ASTPatApp i ps) f = do
    (g,c) <- ask
    case M.lookup i g of
        Just n | S.member n c -> do
            (ps,a) <- foldr (\p b f -> do
                (p,(ps,a)) <- patternBinds p (b f)
                pure (p:ps,a)) (fmap ((,) [])) ps f
            pure (PatternApp n ps,a)
        _ | null ps -> do
            (n,a) <- bind i f
            pure (PatternVar n,a)
        _ -> throwError (InvalidPattern (ASTPatApp i ps))
patternBinds (ASTPatLit l) f = fmap ((,) (PatternLit l)) f

desugarDef :: [Ident] -> Renamer CoreExpr -> Renamer CoreExpr
desugarDef ns e = do
    (ns',e') <- bindMany ns e
    pure (foldr CoreLam e' ns')

desugar :: ASTExpr -> Renamer CoreExpr
desugar = cata go
    where
        go :: ASTExprF (Renamer CoreExpr) -> Renamer CoreExpr
        go (ASTLetF ns e) = foldr (\(n,e) a -> do
            (n',a') <- bind n a
            e' <- e
            pure (CoreLet n' e' a')) e ns
        go (ASTLamF ns e) = do
            (ns',e') <- bindMany ns e
            pure (foldr CoreLam e' ns')
        go (ASTLetRecF fs e) = do
            let (ds,ns) = partitionEithers fs
            (ns',(defs,decs,e')) <- bindMany (fmap (\(f,_,_)->f) ns) $ do
                exp <- e
                decs <- mapM (\(i,t) -> liftM2 (,) (lookupIdent i) (desugarPoly t)) ds
                defs <- mapM (\(_,ns,e) -> desugarDef ns e) ns
                pure (defs,decs,exp)
            defs' <- addAnnots decs (zip ns' defs)
            pure (CoreLetRec defs' e')
        go (ASTAnnotF e t) = liftM2 CoreAnnot e (desugarPoly t)
        go (ASTMatchF e cs) = liftM2 CoreMatch e (mapM (\(p,e) -> patternBinds p e) cs)
        go (ASTAppF f x) = liftM2 CoreApp f x
        go (ASTVarF v) = do
            n <- lookupIdent v
            b <- isCons n
            if b then
                pure (CoreCons n)
            else
                pure (CoreVar n)
        go (ASTPrimopF p x) = fmap (CorePrimop p) (sequence x)
        go (ASTCCallF f x) = fmap (CoreCCall f) (sequence x)

desugarType :: ASTType -> Renamer Type
desugarType = cata go
    where
        go (ASTTyAppF a b) = liftM2 App a b
        go ASTStarF = pure Star
        go ASTArrF = pure Arr
        go (ASTPrimTyF p) = pure (PrimTy p)
        go (ASTTyVarF v) = fmap TyVar (lookupIdent v)

desugarPoly :: ASTType -> Renamer Polytype
desugarPoly t = do
    t' <- desugarType t
    b <- bound
    pure (Forall (S.difference (fv t') b) t')

dataName :: ASTData -> Ident
dataName (ASTGADT i _) = i
dataName (ASTADT i _ _) = i
dataName (ASTStruct i _ _ _) = i

constructors :: ASTData -> [(Ident,ASTType)]
constructors (ASTADT t a cs) =
    fmap (second (\ts -> foldr astArr (foldl ASTTyApp (ASTTyVar t) (fmap ASTTyVar a)) ts)) cs
constructors (ASTStruct t a c ts) =
    [(c,foldr astArr (foldl ASTTyApp (ASTTyVar t) (fmap ASTTyVar a)) (fmap snd ts))]
constructors (ASTGADT t cs) = cs

bindData :: [ASTData] -> Renamer a -> Renamer ([CoreADT],a)
bindData d f = fmap snd . bindMany (fmap dataName d ++ concatMap (fmap fst . constructors) d) $ do
    d' <- forM d $ \d -> do
        n <- lookupIdent (dataName d)
        cs <- mapM (\(i,t) -> liftM2 (,) (lookupIdent i) (desugarPoly t)) (constructors d)
        pure (CoreADT n cs)
    fmap ((,) d') f

partitionTL :: [ASTTL] -> ([ASTData],[(Ident,ASTType)],[(Ident,ASTTLQual)],[ASTDefn])
partitionTL (ASTData d:ts) = (\(x,y,z,w)->(d:x,y,z,w)) (partitionTL ts)
partitionTL (ASTDecl i t:ts) = (\(x,y,z,w)->(x,(i,t):y,z,w)) (partitionTL ts)
partitionTL (ASTQual i q:ts) = (\(x,y,z,w)->(x,y,(i,q):z,w)) (partitionTL ts)
partitionTL (ASTFunc f:ts) = (\(x,y,z,w)->(x,y,z,f:w)) (partitionTL ts)
partitionTL [] = ([],[],[],[])

addAnnots :: [(Name,Polytype)] -> [(Name,CoreExpr)] -> Renamer [(Name,CoreExpr)]
addAnnots m d =
    let d' = fmap (\(n,e) -> case lookup n m of
            Just p -> (n,CoreAnnot e p)
            Nothing -> (n,e)) d
        missing = S.difference (S.fromList (fmap fst m)) (S.fromList (fmap fst d))
    in if S.null missing then
        pure d'
    else
        throwError (MissingDefs (S.toList missing))

resolveQualifiers :: [(Ident,ASTTLQual)] -> ([Ident],[Ident],[Ident])
resolveQualifiers ((i,ASTEntry):qs) = (\(a,b,c)->(i:a,b,c)) (resolveQualifiers qs)
resolveQualifiers ((i,ASTExtern):qs) = (\(a,b,c)->(a,i:b,c)) (resolveQualifiers qs)
resolveQualifiers ((i,ASTExport):qs) = (\(a,b,c)->(a,b,i:c)) (resolveQualifiers qs)
resolveQualifiers [] = ([],[],[])

desugarASTTL :: [ASTTL] -> Renamer CoreMod
desugarASTTL tl =
    let (dat,decl,qual,func) = partitionTL tl
        (entry,extern,exports) = resolveQualifiers qual
        toBind = filter (not . flip elem extern) (fmap fst decl)
    in do
        (data',(func',extern',exports')) <- bindData dat . fmap snd . bindExact exports . fmap snd . bindMany toBind $ do
            decl' <- mapM (\(i,t) -> liftM2 (,) (lookupIdent i) (desugarPoly t)) decl
            func' <- mapM (\(f,a,e) -> liftM2 (,) (lookupIdent f) (desugarDef a (desugar e))) func
            annot <- addAnnots decl' func'
            extern' <- mapM (\i -> do
                n <- lookupIdent i
                case lookup n decl' of
                    Just p -> pure (n,p)
                    Nothing -> throwError (UntypedExtern n)) extern
            exports' <- mapM lookupIdent exports
            pure (annot,extern',exports')
        entry' <- case entry of
            [i] -> fmap Just (lookupIdent i)
            [] -> pure Nothing
            _ -> throwError (MultipleEntry entry)
        pure (CoreMod entry' exports' data' extern' func')

desugarMod :: RenameEnv -> [ASTTL] -> Either NameError CoreMod
desugarMod e tl = runExcept (runReaderT (evalStateT (desugarASTTL tl) mempty) e)