module Frontend.Raw where

import Univ.Name

import Frontend.Common

data RawPattern
    = Uncons Name [(Icit,RawPattern)]
    | MatchLit PrimLit
    | BindVar Name
    deriving(Show)

data Raw
    = Prod Name Icit Raw Raw
    | Abs Name Icit Raw
    | App Icit Raw Raw
    | Match Raw [(RawPattern,Raw)]
    | Cons Name
    | Lit PrimLit
    | Var Name
    | Hole
    | Type
    | Attr Raw Usage
    | Ann Raw Raw
    | Let Name Raw Raw
    | LetRec [(Name,Maybe Raw,Raw)] Raw
    | Primop Primop [Raw]
    deriving(Show)