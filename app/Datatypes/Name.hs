module Datatypes.Name where

import qualified Data.Set as S
import qualified Data.Map as M

type ModulePath = [String]

data Name = Exact String | Export ModulePath String | User ModulePath String Int | Gen Int
    deriving(Eq, Ord)

toLocalName :: Name -> String
toLocalName (Exact s) = s
toLocalName (Export _ s) = s
toLocalName (User _ s _) = s
toLocalName (Gen i) = "v_" ++ show i

toCIdent :: Name -> String
toCIdent (Exact s) = s
toCIdent (Export p s) = concatMap (++"_") p ++ s
toCIdent (User p s i) = concatMap (++"_") p ++ s ++ "_" ++ show i
toCIdent (Gen i) = "v_" ++ show i

instance Show Name where
    show (User p n i) = n ++ ".." ++ show i
    show (Export p n) = n
    show (Exact s) = s
    show (Gen i) = "v." ++ show i

class Free a where
    fv :: a -> S.Set Name

class Subst a where
    subst :: M.Map Name a -> a -> a