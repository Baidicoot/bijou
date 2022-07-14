module Backend (cgen,hgen) where

import Datatypes.ANF
import Datatypes.Pattern
import Datatypes.Prim
import Datatypes.Name
import Control.Monad.RWS
import Datatypes.Core
import Data.List (intercalate)

import qualified Data.Set as S
import qualified Data.Map as M

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
anfValToC (ANFThrow s) = "printf(\"%s\"," ++ show s ++ ")"

type ABIEnv = M.Map Name Int
type CWriter = RWS ABIEnv String Int

getTag :: Name -> CWriter Int
getTag c = do
    e <- ask
    case M.lookup c e of
        Just i -> pure i
        Nothing -> undefined

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

toCOp :: Primop -> String
toCOp Add = "+"
toCOp Sub = "-"
toCOp Mul = "*"
toCOp Div = "/"

anfToC :: ANFExpr -> CWriter ()
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
anfToC (ANFPrimop r p [x,y] k) | boolop p = do
    declare r
    toCIdent r `assign` ("(int)" ++ anfValToC x ++ " " ++ toCOp p ++ " (int)" ++ anfValToC y)
    anfToC k
anfToC (ANFCCall r f a k) = do
    declare r
    toCIdent r `assign` (f ++ "(" ++ intercalate "," (fmap anfValToC a) ++ ")")
    anfToC k
anfToC (ANFReturn v) = writeLn $ "return " ++ anfValToC v ++ ";"
anfToC (ANFMatch x ps d) = do
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

genForwardDecl :: ANFFunc -> CWriter ()
genForwardDecl (ANFFunc f a _) = writeLn $ defHeader f a ++ ";"

genC :: Maybe Name -> String -> [ANFFunc] -> CWriter ()
genC st c d = do
    writeLn "#include <stdlib.h>"
    writeLn "#include <stdint.h>"
    writeLn "typedef void* Ptr;"
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

genH :: String -> [ANFFunc] -> CWriter ()
genH c d = do
    writeLn "#include <stdlib.h>"
    writeLn "#include <stdint.h>"
    writeLn "typedef void* Ptr;"
    mapM_ (\d@(ANFFunc f _ _) -> case f of
        Exact _ -> genForwardDecl d
        _ -> pure ()) d
    writeLn c

cgen :: M.Map Name Int -> CoreMod -> [ANFFunc] -> String
cgen e m@(CoreMod st c _ _ _ _) d = snd (execRWS (genC st c d) e' 0)
    where
        e' = M.union e (consTagsTL m)

hgen :: M.Map Name Int -> CoreMod -> [ANFFunc] -> String
hgen e m d = snd (execRWS (genH (embeddedC m) d) e' 0)
    where
        e' = M.union e (consTagsTL m)