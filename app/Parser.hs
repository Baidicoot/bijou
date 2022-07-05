module Parser (parseLamExpr, parseLamDefs, parseMonotype, parseTLDefs) where

import Datatypes.Lam
import Datatypes.Prim
import Datatypes.Name

import Text.Parsec
import qualified Text.Parsec.Token as Token
import Text.Parsec.Expr
import Text.Parsec.Language (haskellDef)

import Control.Monad
import Control.Monad.Identity

import Data.Bifunctor

import qualified Data.Set as S
import Data.Either (partitionEithers)

type Parser = Parsec String ()
type Op = Operator String () Identity

bijou :: Token.LanguageDef a
bijou = haskellDef
    { Token.reservedNames = Token.reservedNames haskellDef ++
        ["ccall","prim","def","letrec","dec"]
    , Token.reservedOpNames = Token.reservedOpNames haskellDef ++
        [","]
    }

reservedOpNames :: [String]
reservedOpNames = Token.reservedOpNames bijou

lexer :: Token.TokenParser s
lexer = Token.makeTokenParser bijou

parens :: Parser a -> Parser a
parens = Token.parens lexer

angles :: Parser a -> Parser a
angles = Token.angles lexer

identifier :: Parser String
identifier = Token.identifier lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

name :: Parser Name
name = fmap (flip User 0) identifier

variable :: Parser CoreExpr
variable = fmap CoreVar name

intLit :: Parser Int
intLit = fmap fromIntegral (Token.integer lexer)

strLit :: Parser String
strLit = Token.stringLiteral lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

symbol :: String -> Parser String
symbol = Token.symbol lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

toPrimop :: String -> Parser Primop
toPrimop "add" = pure Add
toPrimop "printf" = pure PrintF
toPrimop s = fail ("unknown primop '" ++ s ++ "'")

ccall :: Parser CoreExpr
ccall = reserved "ccall" >> angles (liftM2 CoreCCall identifier (many exprTerm))

primop :: Parser CoreExpr
primop = do
    reserved "prim"
    angles $ do
        op <- toPrimop =<< identifier
        x <- many exprTerm
        pure (CorePrimop op x)

lit :: Parser CoreExpr
lit = fmap (CoreLit . IntLit) intLit <|> fmap (CoreLit . StrLit) strLit

lambda :: Parser CoreExpr
lambda = parens $ do
    reserved "\\"
    ns <- many1 name
    reserved "->"
    fmap (CoreLam ns) expr

letrec :: Parser CoreExpr
letrec = do
    reserved "letrec"
    d <- many1 funcDef
    reserved "in"
    fmap (CoreLetRec d) expr

letval :: Parser CoreExpr
letval = do
    reserved "let"
    n <- name
    reservedOp "="
    d <- expr
    reserved "in"
    fmap (CoreLet n d) expr

exprTerm :: Parser CoreExpr
exprTerm =
    try lambda
    <|> parens expr
    <|> lit
    <|> variable
    <|> letrec
    <|> letval
    <|> primop
    <|> ccall

binary :: String -> (a -> a -> a) -> Assoc -> Op a
binary o f a = flip Infix a $ do
    reservedOp o
    pure f

appExpr :: Op CoreExpr
appExpr = Infix space AssocLeft
    where
        space
            = whiteSpace
            >> notFollowedBy (choice (fmap reservedOp reservedOpNames))
            >> pure CoreApp

expr :: Parser CoreExpr
expr = buildExpressionParser [[appExpr]] exprTerm

arrFn :: Parser Type
arrFn = do
    reserved "(->)"
    pure Arr

litType :: Parser PrimTy
litType =
    fmap (const IntTy) (reserved "Int")
    <|> fmap (const StrTy) (reserved "Str")

monotypeTerm :: Parser Type
monotypeTerm =
    try arrFn
    <|> parens monotype
    <|> fmap PrimTy litType
    <|> fmap TyVar name

appType :: Op Type
appType = Infix space AssocLeft
    where
        space
            = whiteSpace
            >> notFollowedBy (choice (fmap reservedOp reservedOpNames))
            >> pure App

monotype :: Parser Type
monotype = buildExpressionParser [[appType],[binary "->" arr AssocRight]] monotypeTerm

polytype :: Parser Polytype
polytype = do
    t <- monotype
    pure (Forall (fv t) t)

funcDec :: Parser (Name,Polytype)
funcDec = do
    reserved "dec"
    f <- name
    reservedOp "::"
    fmap ((,) f) polytype

funcDef :: Parser (Def CoreExpr)
funcDef = do
    reserved "def"
    f <- name
    a <- many1 name
    reserved "="
    fmap (Def f a Nothing) expr

addHint :: Name -> Polytype -> [Def CoreExpr] -> [Def CoreExpr]
addHint n p (Def f a Nothing e:xs) | f == n = Def f a (Just p) e:xs
addHint n p (x:xs) = x:addHint n p xs
addHint _ _ [] = []

exported :: S.Set Name -> Def a -> (Def a,Bool)
exported n d@(Def f _ _ _) = (d,f `S.member` n)

funcDefs :: Parser [Def CoreExpr]
funcDefs = do
    (decs,defs) <- fmap partitionEithers (many (fmap Left funcDec <|> fmap Right funcDef))
    pure (foldr (uncurry addHint) defs decs)

exportDec :: Parser String
exportDec = do
    reserved "export"
    identifier

tlDefs :: Parser ([Def CoreExpr], S.Set String)
tlDefs = do
    (exports,(decs,defs)) <- fmap (second partitionEithers . partitionEithers)
        (many (fmap Left exportDec <|> fmap (Right . Left) funcDec <|> fmap (Right . Right) funcDef))
    pure (foldr (uncurry addHint) defs decs, S.fromList exports)

parseLamDefs :: String -> String -> Either ParseError [Def CoreExpr]
parseLamDefs = parse funcDefs

parseTLDefs :: String -> String -> Either ParseError ([Def CoreExpr], S.Set String)
parseTLDefs = parse tlDefs

parseLamExpr :: String -> String -> Either ParseError CoreExpr
parseLamExpr = parse expr

parseMonotype :: String -> String -> Either ParseError Type
parseMonotype = parse monotype