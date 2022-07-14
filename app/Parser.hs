module Parser (parseTL) where

import Datatypes.AST
import Datatypes.Prim
import Datatypes.Name

import Text.Parsec
import qualified Text.Parsec.Token as Token
import Text.Parsec.Expr
import Text.Parsec.Language (haskellDef)

import Control.Monad
import Control.Monad.Identity

import Data.Char (isUpper)

import Data.Bifunctor

import qualified Data.Set as S
import Data.Either (partitionEithers)

type Parser = Parsec String ()
type Op = Operator String () Identity

-- TODO: make idents not thingy ->
bijou :: Token.LanguageDef a
bijou = haskellDef
    { Token.reservedNames = Token.reservedNames haskellDef ++
        ["ccall","prim","def","val","letrec","dec","match","with","entry","gadt","struct","extern","export","then"]
    , Token.reservedOpNames = Token.reservedOpNames haskellDef ++
        [",","(->)","->"]
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

lexeme :: Parser a -> Parser a
lexeme = Token.lexeme lexer

name :: Parser Ident
name = fmap Unqualified identifier

variable :: Parser ASTExpr
variable = fmap ASTVar name

intLit :: Parser Int
intLit = fmap fromIntegral (try (Token.integer lexer))

natLit :: Parser Int
natLit = fmap fromIntegral (Token.natural lexer)

strLit :: Parser String
strLit = Token.stringLiteral lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

symbol :: String -> Parser String
symbol = Token.symbol lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

toPrimop :: String -> Parser Primop
toPrimop = go primops
    where
        go ((t,p):ps) s | s == t = pure p
        go (_:ps) s = go ps s
        go [] s = fail ("unknown primop " ++ s)

primops :: [(String,Primop)]
primops =
    [("add",Add)
    ,("sub",Sub)
    ,("mul",Mul)
    ,("div",Div)
    ]

ccall :: Parser ASTExpr
ccall = reserved "ccall" >> angles (liftM2 ASTCCall identifier (many exprTerm))

cstring :: Parser String
cstring = reservedOp "<<<" >> manyTill anyChar (try (reservedOp ">>>"))

embedC :: Parser String
embedC = lexeme $ do
    reserved "ccall"
    cstring

primop :: Parser ASTExpr
primop = do
    reserved "prim"
    angles $ do
        op <- toPrimop =<< identifier
        x <- many exprTerm
        pure (ASTPrimop op x)

patternExpr :: Parser ASTPattern
patternExpr =
    try (liftM2 ASTPatApp name (many patternTerm))
    <|> patternTerm

patternTerm :: Parser ASTPattern
patternTerm =
    parens patternExpr
    <|> fmap (flip ASTPatApp []) name
    <|> fmap ASTPatLit lit

match :: Parser ASTExpr
match = do
    reserved "case"
    x <- expr
    reserved "of"
    optional (reservedOp "|")
    cs <- flip sepBy (reservedOp "|") $ do
        p <- patternExpr
        reservedOp "->"
        fmap ((,) p) expr
    pure (ASTMatch x cs)

lit :: Parser Lit
lit = fmap IntLit intLit <|> fmap StrLit strLit

litExpr :: Parser ASTExpr
litExpr = fmap ASTLit lit

lambda :: Parser ASTExpr
lambda = parens $ do
    reservedOp "\\"
    ns <- many1 name
    reservedOp "->"
    fmap (ASTLam ns) expr

dothen :: Parser ASTExpr
dothen = do
    reserved "do"
    exprs <- flip sepBy (reserved "then") expr
    reserved "in"
    fmap (ASTDoThen exprs) expr

letrec :: Parser ASTExpr
letrec = do
    reserved "letrec"
    d <- funcDefs
    reserved "in"
    fmap (ASTLetRec d) expr

letval :: Parser ASTExpr
letval = do
    reserved "let"
    ds <- many1 $ do
        reserved "val"
        n <- name
        reservedOp "="
        fmap ((,) n) expr
    reserved "in"
    fmap (ASTLet ds) expr

exprTerm :: Parser ASTExpr
exprTerm =
    try lambda
    <|> parens expr
    <|> dothen
    <|> match
    <|> litExpr
    <|> variable
    <|> letrec
    <|> letval
    <|> primop
    <|> ccall

binary :: String -> (a -> a -> a) -> Assoc -> Op a
binary o f a = flip Infix a $ do
    reservedOp o
    pure f

appExpr :: Op ASTExpr
appExpr = Infix space AssocLeft
    where
        space
            = whiteSpace
            >> notFollowedBy (choice (fmap reservedOp reservedOpNames))
            >> pure ASTApp

expr :: Parser ASTExpr
expr = buildExpressionParser [[appExpr]] exprTerm

arrFn :: Parser ASTType
arrFn = do
    reserved "(->)"
    pure ASTArr

litType :: Parser PrimTy
litType =
    fmap (const IntTy) (reserved "Int")
    <|> fmap (const StrTy) (reserved "Str")

monotypeTerm :: Parser ASTType
monotypeTerm =
    try arrFn
    <|> parens monotype
    <|> fmap ASTPrimTy litType
    <|> fmap ASTTyVar name

appType :: Op ASTType
appType = Infix space AssocLeft
    where
        space
            = whiteSpace
            >> notFollowedBy (choice (fmap reservedOp reservedOpNames))
            >> pure ASTTyApp

monotype :: Parser ASTType
monotype = buildExpressionParser [[appType],[binary "->" astArr AssocRight]] monotypeTerm

funcDec :: Parser (Ident,ASTType)
funcDec = do
    reserved "dec"
    f <- name
    reservedOp "::"
    fmap ((,) f) monotype

funcDef :: Parser (Ident,[Ident],ASTExpr)
funcDef = do
    reserved "def"
    f <- name
    a <- many1 name
    reservedOp "="
    fmap (\e->(f,a,e)) expr

gadtData :: Parser ASTData
gadtData = do
    reserved "gadt"
    f <- name
    reservedOp "="
    optional (reservedOp "|")
    c <- flip sepBy (reservedOp "|") $ do
        c <- name
        reserved "::"
        fmap ((,) c) monotype
    pure (ASTGADT f c)

adtData :: Parser ASTData
adtData = do
    reserved "data"
    f <- name
    a <- many name
    reservedOp "="
    optional (reservedOp "|")
    c <- flip sepBy (reservedOp "|") $ do
        c <- name
        fmap ((,) c) (many monotypeTerm)
    pure (ASTADT f a c)

structData :: Parser ASTData
structData = do
    reserved "struct"
    f <- name
    a <- many name
    reservedOp "="
    c <- name
    optional (reservedOp "|")
    d <- flip sepBy (reservedOp "|") $ do
        d <- name
        reservedOp "::"
        fmap ((,) d) monotype
    pure (ASTStruct f a c d)

funcDefs :: Parser [Either (Ident,ASTType) (Ident,[Ident],ASTExpr)]
funcDefs = many (fmap Left funcDec <|> fmap Right funcDef)

exportDec :: Parser (Ident,ASTTLQual)
exportDec = do
    reserved "export"
    a <- natLit
    i <- name
    pure (i,ASTExport a)

externDec :: Parser (Ident,ASTTLQual)
externDec = do
    reserved "extern"
    a <- natLit
    i <- name
    pure (i,ASTExtern a)

entryDec :: Parser (Ident,ASTTLQual)
entryDec = do
    reserved "entry"
    i <- name
    pure (i,ASTEntry)

tlStmts :: Parser [ASTTL]
tlStmts = many $
    fmap (uncurry ASTQual) exportDec
    <|> fmap (uncurry ASTQual) entryDec
    <|> fmap (uncurry ASTQual) externDec
    <|> fmap (uncurry ASTDecl) funcDec
    <|> fmap ASTFunc funcDef
    <|> fmap ASTEmbC embedC
    <|> fmap ASTData (gadtData <|> adtData <|> structData)

parseTL :: String -> String -> Either ParseError [ASTTL]
parseTL = parse tlStmts