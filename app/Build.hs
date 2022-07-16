module Build where

import Datatypes.AST
import Datatypes.Core
import Datatypes.Build
import Datatypes.Name
import ANFify
import Backend
import ClosureConv
import Desugar
import Parser
import PartialApp
import Typecheck

import Data.List (intercalate)
import Data.Maybe (mapMaybe)
import qualified Data.Map as M
import qualified Data.Set as S

genRenamingMap :: M.Map ModulePath Module -> Import -> M.Map Ident Name
genRenamingMap mods (Import p) = case M.lookup p mods of
    Just m -> M.fromList (fmap (\n -> (Unqualified (toLocalName n),n)) (S.toList (modNames m)))
    Nothing -> M.empty
genRenamingMap mods (ImportQual p q) = case M.lookup p mods of
    Just m -> M.fromList (fmap (\n -> (Qualified q (toLocalName n),n)) (S.toList (modNames m)))
    Nothing -> M.empty

fromEitherM :: Monad m => Either a b -> (a -> m b) -> m b
fromEitherM (Left e) f = f e
fromEitherM (Right a) f = pure a

toBuildPath :: ModulePath -> FilePath
toBuildPath p = intercalate "_" p

toSrcFile :: ModulePath -> FilePath
toSrcFile p = intercalate "/" p ++ ".bij"

buildModule :: FilePath -> FilePath -> [(ModulePath,Module)] -> ModulePath -> IO Module
buildModule root build m p = do
    putStrLn ("Compiling '" ++ intercalate "." p ++ "'...")
    let path = root ++ "/" ++ toSrcFile p
    file <- readFile path
    (imp,tl) <- fromEitherM (parseTL path file) (ioError . userError . show)
    let rm = M.unions (fmap (genRenamingMap (M.fromList m)) imp)
    let currCons = S.unions (fmap (modCons . snd) m)
    let currTypes = S.unions (fmap (modConsts . snd) m)
    let globals = S.unions (fmap (modNames . snd) m)
    cm <- fromEitherM (desugarMod (rm,currCons,currTypes,p) tl) (ioError . userError . show)
    let gamma = M.unions (fmap (modTypes . snd) m)
    print (exportNamesTL cm)
    (t,s) <- fromEitherM (fst (runInferMod 0 gamma cm)) (ioError . userError . show)
    let ((ld,ar),s') = cconvDefs globals s cm
    let arities = M.unions (fmap (modArities . snd) m)
    let reprs = M.unions (fmap (modReprs . snd) m)
    let (s'',pd) = partialsMod s' reprs arities cm ld
    let (ad,s''') = anfifyDefs s'' pd
    let headers = fmap ((++".h") . toBuildPath . importMod) imp
    let cFileCont = cgen headers cm ad
    let hFileCont = hgen cm ad
    let buildPath = build ++ "/" ++ toBuildPath p
    writeFile (buildPath ++ ".c") cFileCont
    writeFile (buildPath ++ ".h") hFileCont
    let exp = mapMaybe (\(n,p) -> case lookup n ar of
            Just a | elem n (funcs (exportNamesTL cm)) -> Just (n,p,a)
            _ -> Nothing) t
    pure (Module (datasMod cm) exp (cons (exportNamesTL cm)) (types (exportNamesTL cm)))