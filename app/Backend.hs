module Backend (cgen,hgen) where

import Datatypes.ANF
import Datatypes.Pattern
import Datatypes.Prim
import Datatypes.Name
import Control.Monad.Writer
import Control.Monad.State
import Datatypes.Core
import Data.List (intercalate)

import qualified Data.Set as S
import qualified Data.Map as M

anfValToC :: ANFVal -> String
anfValToC (ANFVar n) = toCIdent n
anfValToC (ANFLabel n) = toCIdent n
anfValToC (ANFLit l) = show l
anfValToC (ANFRef n) = '&':toCIdent n
anfValToC (ANFThrow s) = "printf(\"%s\"," ++ show s ++ ")"

type CWriter = WriterT String (State Int)

indent :: Int -> CWriter ()
indent = put

indentBy :: Int -> CWriter ()
indentBy n = modify (+n)

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

call :: String -> [String] -> String
call f a = f ++ "(" ++ intercalate "," a ++ ")"

toCOp :: Primop -> String
toCOp Add = "+"
toCOp Sub = "-"
toCOp Mul = "*"
toCOp Div = "/"

anfToC :: ANFExpr -> CWriter ()
{-
anfToC (ANFMkClosure r l a k) = do
    declare r
    toCIdent r `assign` ("malloc(sizeof(Ptr)*" ++ show (length a + 1) ++ ")")
    forM_ (zip (ANFLabel l:a) [0..]) $ \(a,i) ->
        index (toCIdent r) i `assign` anfValToC a
    anfToC k
anfToC (ANFMkCons r c a k) = do
    declare r
    toCIdent r `assign` ("malloc(sizeof(Ptr)*" ++ show (length a + 1) ++ ")")
    t <- getTag c
    forM_ (zip (ANFLit (IntLit t):a) [0..]) $ \(a,i) ->
        index (toCIdent r) i `assign` anfValToC a
    anfToC k
-}
anfToC (ANFMkRecord r a k) = do
    declare r
    toCIdent r `assign` ("malloc(sizeof(Ptr)*" ++ show (length a) ++ ")")
    forM_ (zip a [0..]) $ \(a,i) ->
        index (toCIdent r) i `assign` anfValToC a
    anfToC k
anfToC (ANFLet r v k) = do
    declare r
    toCIdent r `assign` anfValToC v
    anfToC k
anfToC (ANFIndexRecord v i r k) = do
    declare v
    toCIdent v `assign` index (anfValToC r) i
    anfToC k
anfToC (ANFAppLocal r c a k) = do
    declare r
    let f = "((Ptr (*)(Ptr,Ptr))(" ++ anfValToC c ++ "))"
    toCIdent r `assign` call f (fmap anfValToC a)
    anfToC k
anfToC (ANFAppGlobal r f a k) = do
    declare r
    toCIdent r `assign` call (toCIdent f) (fmap anfValToC a)
    anfToC k
anfToC (ANFPrimop r p [x,y] k) | boolop p = do
    declare r
    toCIdent r `assign` ("(int)" ++ anfValToC x ++ " " ++ toCOp p ++ " (int)" ++ anfValToC y)
    anfToC k
anfToC (ANFCCall r f a k) = do
    declare r
    toCIdent r `assign` call f (fmap anfValToC a)
    anfToC k
anfToC (ANFReturn v) = writeLn $ "return " ++ anfValToC v ++ ";"
anfToC (ANFSwitch _ [] d) = anfToC d
anfToC (ANFSwitch x ((l,k):cs) d) = do
    writeLn ("if (" ++ anfValToC x ++ " == " ++ show l ++ ") {")
    indentBy 4
    anfToC k
    indentBy (-4)
    forM_ cs $ \(l,k) -> do
        writeLn ("} else if (" ++ anfValToC x ++ " == " ++ show l ++ ") {")
        indentBy 4
        anfToC k
        indentBy (-4)
    writeLn "} else {"
    indentBy 4
    anfToC d
    indentBy (-4)
    writeLn "}"
{-
anfToC (ANFSwitch x ps d) = do
    cases <- forM ps $ \(p,k) -> case p of
        FlatPatternLit l -> pure (anfValToC x ++ " == " ++ anfValToC (ANFLit l),[],k)
        FlatPatternApp c v -> fmap (\t -> (index (anfValToC x) 0 ++ " == " ++ show t,v,k)) (getTag c)
    case cases of
        [] -> anfToC d
        [(s,v,k)] -> do
            writeLn ("if (" ++ s ++ ") {")
            writeCase v k
            writeLn "} else {"
            writeCase [] d
            writeLn "}"
        ((s,v,k):cs) -> do
            writeLn ("if (" ++ s ++ ") {")
            writeCase v k
            forM_ cs $ \(s,v,k) -> do
                writeLn ("} else if (" ++ s ++ ") {")
                writeCase v k
            writeLn "} else {"
            writeCase [] d
            writeLn "}"
    where
        writeCase v k = do
            indentBy 4
            forM_ (zip v [1..]) $ \(r,i) -> do
                declare r
                toCIdent r `assign` index (anfValToC x) i
            anfToC k
            indentBy (-4)
-}

anfToC _ = error "undefined"

defHeader :: Name -> [Name] -> String
defHeader f a = "Ptr " ++ toCIdent f ++ "(" ++ intercalate "," (fmap (("Ptr "++) . toCIdent) a) ++ ")"

anfDefToC :: ANFFunc -> CWriter ()
anfDefToC (ANFFunc f a e) = do
    writeLn $ defHeader f a ++ " {"
    indent 4
    anfToC e
    indent 0
    writeLn "}"
anfDefToC (ANFConst n l) = writeLn ("Ptr " ++ toCIdent n ++ " = " ++ show l ++ ";")

genForwardDecl :: ANFFunc -> CWriter ()
genForwardDecl (ANFFunc f a _) = writeLn $ defHeader f a ++ ";"
genForwardDecl (ANFConst n _) = writeLn ("Ptr " ++ toCIdent n ++ ";")

defName :: ANFFunc -> Name
defName (ANFFunc f _ _) = f
defName (ANFConst n _) = n

genC :: [String] -> Maybe Name -> String -> [ANFFunc] -> CWriter ()
genC inc st c d = do
    writeLn "#include <stdlib.h>"
    writeLn "#include <stdint.h>"
    writeLn "typedef void* Ptr;"
    mapM_ (\f -> writeLn ("#include \"" ++ f ++ "\"")) inc
    mapM_ genForwardDecl d
    writeLn c
    case st of
        Just st -> do
            writeLn "void main(int main_arg) {"
            indent 4
            writeLn (toCIdent st ++ "(main_arg);")
            indent 0
            writeLn "}"
        Nothing -> pure ()
    mapM_ anfDefToC d

genH :: S.Set Name -> [ANFFunc] -> CWriter ()
genH exp d = do
    writeLn "#include <stdlib.h>"
    writeLn "#include <stdint.h>"
    writeLn "typedef void* Ptr;"
    mapM_ (\d ->
        if S.member (defName d) exp then
            genForwardDecl d
        else
            pure ()) d

cgen :: [String] -> CoreMod -> [ANFFunc] -> String
cgen inc m d = evalState (execWriterT (genC inc (startName m) (embeddedC m) d)) 0

hgen :: CoreMod -> [ANFFunc] -> String
hgen m d = evalState (execWriterT (genH x d)) 0
    where
        x = S.fromList (cons (exportNamesTL m) ++ funcs (exportNamesTL m))