module Backend (cgen) where

import Datatypes.Lam
import Datatypes.Prim
import Control.Monad.Writer
import Control.Monad.State
import Data.List (intercalate)

toCIdent :: Name -> String
toCIdent = sanitize . show

sanitize :: String -> String
sanitize = concatMap sanitizeChar
    where
        sanitizeChar '.' = "_"
        sanitizeChar '_' = "_underscore_"
        sanitizeChar c = [c]

anfValToC :: ANFVal -> String
anfValToC (ANFVar n) = toCIdent n
anfValToC (ANFLabel n) = toCIdent n
anfValToC (ANFLit l) = show l

type CWriter = WriterT String (State Int)

indent :: Int -> CWriter ()
indent = put

writeLn :: String -> CWriter ()
writeLn s = do
    i <- get
    tell (replicate i ' ')
    tell (s ++ "\n")

declare :: Name -> CWriter ()
declare n = writeLn $ "Ptr " ++ toCIdent n ++ ";"

assign :: String -> String -> CWriter ()
assign r v = writeLn $ r ++ " = " ++ v ++ ";"

index :: String -> Int -> String
index s i = "((Ptr*)(" ++ s ++ "))[" ++ show i ++ "]"

anfToC :: ANFExpr -> CWriter ()
anfToC (ANFMkClosure r l a k) = do
    declare r
    toCIdent r `assign` ("malloc(sizeof(Ptr)*" ++ show (length a + 1) ++ ")")
    forM_ (zip (ANFLabel l:a) [0..]) $ \(a,i) ->
        index (toCIdent r) i `assign` anfValToC a
    anfToC k
anfToC (ANFLet r v k) = do
    declare r
    toCIdent r `assign` anfValToC v
    anfToC k
anfToC (ANFUnpackPartial r v k) = do
    forM_ (zip r [1..]) $ \(r,i) -> do
        declare r
        toCIdent r `assign` index (anfValToC v) i
    anfToC k
anfToC (ANFAppPartial r c a k) = do
    declare r
    toCIdent r `assign` ("(((Ptr (**)(Ptr,Ptr))(" ++ anfValToC c ++ "))[0])(" ++ anfValToC c ++ "," ++ anfValToC a ++ ")")
    anfToC k
anfToC (ANFAppGlobal r f a k) = do
    declare r
    toCIdent r `assign` (toCIdent f ++ "(" ++ intercalate "," (fmap anfValToC a) ++ ")")
    anfToC k
anfToC (ANFPrimop r Add [x,y] k) = do
    declare r
    toCIdent r `assign` ("(int)" ++ anfValToC x ++ " + (int)" ++ anfValToC y)
    anfToC k
anfToC (ANFCCall r f a k) = do
    declare r
    toCIdent r `assign` (f ++ "(" ++ intercalate "," (fmap anfValToC a) ++ ")")
    anfToC k
anfToC (ANFReturn v) = writeLn $ "return " ++ anfValToC v ++ ";"
anfToC _ = error "undefined"

defHeader :: Name -> [Name] -> String
defHeader f a = "Ptr " ++ toCIdent f ++ "(Ptr " ++ intercalate ",Ptr " (fmap toCIdent a) ++ ")"

anfDefToC :: Def ANFExpr -> CWriter ()
anfDefToC (Def f a e) = do
    writeLn $ defHeader f a ++ " {"
    indent 4
    anfToC e
    indent 0
    writeLn "}"

genForwardDecl :: Def ANFExpr -> CWriter ()
genForwardDecl (Def f a _) = writeLn $ defHeader f a ++ ";"

genC :: [Def ANFExpr] -> CWriter ()
genC d = do
    writeLn "#include <stdlib.h>"
    writeLn "#include <stdint.h>"
    writeLn "typedef void* Ptr;"
    mapM_ genForwardDecl d
    mapM_ anfDefToC d

cgen :: [Def ANFExpr] -> String
cgen = flip evalState 0 . execWriterT . genC