module Datatypes.Name where

import qualified Data.Set as S

data Name = Exact String | User String Int | Gen Int
    deriving(Eq, Ord)

instance Show Name where
    show (User n i) = n ++ ".." ++ show i
    show (Exact s) = s
    show (Gen i) = "v." ++ show i

class Free a where
    fv :: a -> S.Set Name