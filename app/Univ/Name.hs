module Univ.Name where

import qualified Data.Set as S
import qualified Data.Map as M

type ModulePath = [String]

data Name = Exact String | User ModulePath String Int | Gen Int
    deriving(Eq, Ord, Show)

toLocalName :: Name -> String
toLocalName (Exact s) = s
toLocalName (User _ s _) = s
toLocalName (Gen i) = "v_" ++ show i

sanitize :: String -> String
sanitize = concatMap go
    where
        go '\'' = "_prime"
        go '_' = "__"
        go c = [c]

toCIdent :: Name -> String
toCIdent (Exact s) = s
toCIdent (User p s i) = concatMap (++"_") p ++ sanitize s ++ "_" ++ show i
toCIdent (Gen i) = "v_" ++ show i

class Free a where
    fv :: a -> S.Set Name

class Subst a where
    subst :: M.Map Name a -> a -> a