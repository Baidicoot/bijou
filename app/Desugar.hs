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
import Data.List (partition)
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Foldable as F

-- counter for each variable
type RenameState = M.Map Ident Int
-- imports + locally bound names, constructors, constant types, path
type RenameEnv = (M.Map Ident Name,S.Set Name,S.Set Name,ModulePath)

data NameError
    = UnknownIdent Ident
    | InvalidPattern ASTPattern
    | MissingDefs [Name]
    | UntypedExtern Name
    | MultipleEntry [Ident]
    deriving(Show)

type Renamer = StateT RenameState (ReaderT RenameEnv (Except NameError))

identToStr :: Ident -> String
identToStr (Unqualified s) = s
identToStr (Qualified _ s) = s
identToStr Hole = "_"

path :: Renamer ModulePath
path = asks (\(_,_,_,p)->p)

fresh :: Ident -> Renamer Name
fresh s = do
    g <- get
    (_,_,_,p) <- ask
    let i = M.findWithDefault 0 s g
    put (M.insert s (i+1) g)
    pure (User p (identToStr s) i)

lookupIdent :: Ident -> Renamer Name
lookupIdent s = do
    (g,_,_,_) <- ask
    case M.lookup s g of
        Just n -> pure n
        Nothing -> throwError (UnknownIdent s)

isCons :: Name -> Renamer Bool
isCons n = do
    (_,c,_,_) <- ask
    pure (S.member n c)

isConst :: Name -> Renamer Bool
isConst n = do
    (_,_,c,_) <- ask
    pure (S.member n c)

withVars :: [(Ident,Name)] -> Renamer a -> Renamer a
withVars m = local (\(i,c,t,p) -> (M.union (M.fromList m) i,c,t,p))

bound :: Renamer (S.Set Ident)
bound = do
    (g,_,_,_) <- ask
    pure (M.keysSet g)

withCons :: S.Set Name -> Renamer a -> Renamer a
withCons c' = local (\(i,c,t,p)->(i,S.union c c',t,p))

withConsts :: S.Set Name -> Renamer a -> Renamer a
withConsts t' = local (\(i,c,t,p)->(i,c,S.union t t',p))

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

{-
bindExport :: [Ident] -> Renamer a -> Renamer ([Name],a)
bindExport is f = do
    p <- path
    let is' = fmap (Export p . identToStr) is
    fmap ((,) is') (withVars (zip is is') f)
-}

patternBinds :: ASTPattern -> Renamer a -> Renamer (Pattern,a)
patternBinds (ASTPatApp i ps) f = do
    (g,c,_,_) <- ask
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
        go (ASTLitF l) = pure (CoreLit l)
        go (ASTPrimopF p x) = fmap (CorePrimop p) (sequence x)
        go (ASTCCallF f x) = fmap (CoreCCall f) (sequence x)
        go (ASTDoThenF xs x) = do
            xs' <- mapM (liftM2 (,) (fresh Hole)) xs
            x' <- x
            pure (foldr (uncurry CoreLet) x' xs')

desugarType :: ASTType -> Renamer Type
desugarType = cata go
    where
        go (ASTTyAppF a b) = liftM2 App a b
        go ASTStarF = pure Star
        go ASTArrF = pure Arr
        go (ASTPrimTyF p) = pure (PrimTy p)
        go (ASTWeakVarF v) = fmap Weak (lookupIdent v)
        go (ASTTyVarF v) = do
            n <- lookupIdent v
            b <- isConst n
            if b then
                pure (ConstTy n)
            else
                pure (TyVar n)

astTypeFree :: S.Set Ident -> ASTType -> S.Set Ident
astTypeFree b t = S.difference (cata go t) b
    where
        go (ASTTyVarF v) = S.singleton v
        go (ASTWeakVarF v) = S.singleton v
        go x = F.fold x

desugarPoly :: ASTPoly -> Renamer Polytype
desugarPoly (ASTPolyImplicit t) = do
    b <- bound
    (n,t') <- bindMany (S.toList (astTypeFree b t)) (desugarType t)
    pure (Forall (S.fromList n) t')
desugarPoly (ASTPolyExplicit is t) = do
    (n,t') <- bindMany is (desugarType t)
    pure (Forall (S.fromList n) t')

bindData :: [ASTData] -> Renamer a -> Renamer ([CoreADT],a)
bindData d f = fmap snd . bindMany (fmap astDataType d ++ concatMap astDataConsNames d) $ do
    d' <- forM d $ \d -> do
        n <- lookupIdent (astDataType d)
        cs <- mapM (\(i,t) -> liftM2 (,) (lookupIdent i) (desugarPoly t)) (astDataCons d)
        pure (CoreADT n cs)
    let cons = mconcat (fmap consNames d')
    let consts = S.fromList (fmap typeName d')
    fmap ((,) d') (withCons cons (withConsts consts f))

partitionTL :: [ASTTL] -> ([ASTData],[(Ident,ASTPoly)],[ASTTLQual],[ASTDefn],String)
partitionTL (ASTData d:ts) = (\(x,y,z,w,v)->(d:x,y,z,w,v)) (partitionTL ts)
partitionTL (ASTDecl i t:ts) = (\(x,y,z,w,v)->(x,(i,t):y,z,w,v)) (partitionTL ts)
partitionTL (ASTQual q:ts) = (\(x,y,z,w,v)->(x,y,q:z,w,v)) (partitionTL ts)
partitionTL (ASTFunc f:ts) = (\(x,y,z,w,v)->(x,y,z,f:w,v)) (partitionTL ts)
partitionTL (ASTEmbC c:ts) = (\(x,y,z,w,v)->(x,y,z,w,c++v)) (partitionTL ts)
partitionTL [] = ([],[],[],[],"")

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

resolveQualifiers :: [ASTTLQual] -> ([Ident],[(Ident,Int)],[Ident])
resolveQualifiers (ASTEntry i:qs) = (\(a,b,c)->(i:a,b,c)) (resolveQualifiers qs)
resolveQualifiers (ASTExtern i r:qs) = (\(a,b,c)->(a,(i,r):b,c)) (resolveQualifiers qs)
resolveQualifiers (ASTExport i:qs) = (\(a,b,c)->(a,b,i++c)) (resolveQualifiers qs)
resolveQualifiers [] = ([],[],[])

desugarASTTL :: [ASTTL] -> Renamer CoreMod
desugarASTTL tl =
    let (dat,decl,qual,func,embedded) = partitionTL tl
        (entry,extern,exports) = resolveQualifiers qual
        externNames = fmap fst extern
        typeNames = fmap astDataType dat
        consNames = concatMap astDataConsNames dat
        exportFuncs = filter (not . flip elem (typeNames ++ consNames)) exports
        exportCons = filter (flip elem consNames) exports
        exportTypes = filter (flip elem typeNames) exports
        toBind = filter (not . flip elem externNames) (fmap fst decl)
    in do
        (data',(func',extern',exports',entry')) <- bindData dat . fmap snd . bindExact externNames . fmap snd . bindMany toBind $ do
            let (indecl,outdecl) = partition (not . flip elem externNames . fst) decl
            indecl' <- mapM (\(i,t) -> liftM2 (,) (lookupIdent i) (desugarPoly t)) indecl
            outdecl' <- mapM (\(i,t) -> liftM2 (,) (lookupIdent i) (desugarPoly t)) outdecl
            let decl' = indecl' ++ outdecl'
            func' <- mapM (\(f,a,e) -> liftM2 (,) (lookupIdent f) (desugarDef a (desugar e))) func
            annot <- addAnnots indecl' func'
            extern' <- mapM (\(i,r) -> do
                n <- lookupIdent i
                case lookup n decl' of
                    Just p -> pure (n,(r,p))
                    Nothing -> throwError (UntypedExtern n)) extern
            exportFuncs' <- mapM lookupIdent exportFuncs
            exportCons' <- mapM lookupIdent exportCons
            exportTypes' <- mapM lookupIdent exportTypes
            entry' <- case entry of
                [i] -> fmap Just (lookupIdent i)
                [] -> pure Nothing
                _ -> throwError (MultipleEntry entry)
            pure (annot,extern',(Exports exportCons' exportTypes' exportFuncs'),entry')
        pure (CoreMod entry' embedded exports' data' extern' func')

desugarMod :: RenameEnv -> [ASTTL] -> Either NameError CoreMod
desugarMod e tl = runExcept (runReaderT (evalStateT (desugarASTTL tl) mempty) e)