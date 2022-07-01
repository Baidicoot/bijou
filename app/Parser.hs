module Parser (parseLamExpr, parseLamDefs) where

import Datatypes.Lam
import Datatypes.Prim

import Text.Parsec
import qualified Text.Parsec.Token as Token
import Text.Parsec.Combinator
import Text.Parsec.Language (haskellDef)

import Control.Monad

type Parser = Parsec String ()

lexer :: Token.TokenParser s
lexer = Token.makeTokenParser $ haskellDef
    { Token.reservedNames = Token.reservedNames haskellDef ++
        ["ccall","prim","def","letrec"]
    }

parens :: Parser a -> Parser a
parens = Token.parens lexer

angles :: Parser a -> Parser a
angles = Token.angles lexer

identifier :: Parser String
identifier = Token.identifier lexer

name :: Parser Name
name = fmap (flip User 0) identifier

variable :: Parser CoreExpr
variable = fmap CoreVar name

intLit :: Parser Int
intLit = fmap fromIntegral (Token.integer lexer)

strLit :: Parser String
strLit = Token.stringLiteral lexer

symbol :: String -> Parser String
symbol = Token.symbol lexer

toPrimop :: String -> Parser Primop
toPrimop "add" = pure Add
toPrimop "printf" = pure PrintF
toPrimop s = fail ("unknown primop '" ++ s ++ "'")

ccall :: Parser CoreExpr
ccall = symbol "ccall" >> angles (liftM2 CoreCCall identifier (many appExpr))

primop :: Parser CoreExpr
primop = do
    symbol "prim"
    angles $ do
        op <- toPrimop =<< identifier
        x <- many appExpr
        pure (CorePrimop op x)

lit :: Parser CoreExpr
lit = fmap (CoreLit . Int) intLit <|> fmap (CoreLit . String) strLit

lambda :: Parser CoreExpr
lambda = parens $ do
    symbol "\\"
    ns <- many1 name
    symbol "->"
    fmap (CoreLam ns) expr

letrec :: Parser CoreExpr
letrec = do
    symbol "letrec"
    d <- many1 funcDef
    symbol "in"
    fmap (CoreLetRec d) expr

letval :: Parser CoreExpr
letval = do
    symbol "let"
    n <- name
    symbol "="
    d <- expr
    symbol "in"
    fmap (CoreLet n d) expr

appExpr :: Parser CoreExpr
appExpr =
    try lambda
    <|> parens expr
    <|> lit
    <|> variable
    <|> letrec
    <|> letval
    <|> primop
    <|> ccall

application :: Parser CoreExpr
application = fmap (foldl1 CoreApp) (many1 appExpr)

expr :: Parser CoreExpr
expr = application <|> appExpr

funcDef :: Parser (Def CoreExpr)
funcDef = do
    symbol "def"
    f <- name
    a <- many1 name
    symbol "="
    fmap (Def f a) expr

parseLamDefs :: String -> String -> Either ParseError [Def CoreExpr]
parseLamDefs = parse (many funcDef)

parseLamExpr :: String -> String -> Either ParseError CoreExpr
parseLamExpr = parse expr