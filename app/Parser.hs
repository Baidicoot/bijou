module Parser (parseLamExpr) where

import Datatypes.Lam

import Text.Parsec
import qualified Text.Parsec.Token as Token
import Text.Parsec.Combinator
import Text.Parsec.Language (haskellDef)

import qualified Data.Text as T

type Parser = Parsec String ()

lexer = Token.makeTokenParser haskellDef

parens :: Parser a -> Parser a
parens = Token.parens lexer

identifier :: Parser String
identifier = Token.identifier lexer

name :: Parser Name
name = fmap (flip User 0 . T.pack) identifier

variable :: Parser CoreExpr
variable = fmap CoreVar name

symbol :: String -> Parser String
symbol = Token.symbol lexer

lambda :: Parser CoreExpr
lambda = parens $ do
    symbol "\\"
    ns <- many1 name
    symbol "->"
    fmap (CoreLam ns) expr

appExpr :: Parser CoreExpr
appExpr = try lambda <|> parens expr <|> variable

application :: Parser CoreExpr
application = fmap (foldl1 CoreApp) (many1 appExpr)

expr :: Parser CoreExpr
expr = application <|> appExpr

parseLamExpr :: String -> String -> Either ParseError CoreExpr
parseLamExpr = parse expr