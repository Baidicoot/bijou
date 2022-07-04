module Datatypes.Name where

import qualified Data.Set as S

data Name = User String Int | Gen Int
    deriving(Eq, Ord)

instance Show Name where
    show (User n 0) = n
    show (User n i) = n ++ ".." ++ show i
    show (Gen i) = "v." ++ show i

class Free a where
    fv :: a -> S.Set Name