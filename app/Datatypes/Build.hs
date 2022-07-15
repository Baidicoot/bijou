module Datatypes.Build where

import Datatypes.Core
import Datatypes.Type
import Datatypes.Name

import qualified Data.Map as M
import qualified Data.Set as S

data Import
    = ImportQual ModulePath String
    | Import ModulePath

importMod :: Import -> ModulePath
importMod (ImportQual p _) = p
importMod (Import p) = p

data Module = Module
    { dataTypes :: [CoreADT]
    , exportFuncs :: [(Name,Polytype,Int)]
    , exportCons :: [Name]
    , exportTypes :: [Name]
    }

modArities :: Module -> M.Map Name Int
modArities (Module d e c _) = M.unions (M.fromList (fmap (\(n,_,a)->(n,a)) e)
    :fmap (M.filterWithKey (const . flip elem c) . consArities) d)

modTypes :: Module -> M.Map Name Polytype
modTypes (Module d e c _) = M.unions (M.fromList (fmap (\(n,p,_)->(n,p)) e)
    :fmap (M.filterWithKey (const . flip elem c) . consTypes) d)

modTags :: Module -> M.Map Name Int
modTags (Module d _ _ _) = M.unions (fmap consTags d)

modCons :: Module -> S.Set Name
modCons (Module _ _ c _) = S.fromList c

modConsts :: Module -> S.Set Name
modConsts (Module _ _ _ t) = S.fromList t

modNames :: Module -> S.Set Name
modNames (Module _ f c t) = S.fromList (fmap (\(n,_,_)->n) f ++ c ++ t)