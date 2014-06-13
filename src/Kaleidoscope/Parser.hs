module Kaleidoscope.Parser
    ( parseExpr
    , parseToplevel
    ) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Token
import Kaleidoscope.AST


lexer :: TokenParser ()
lexer = makeTokenParser
    emptyDef { commentLine     = "#"
             , reservedOpNames = ["+","*","-",";"]
             , reservedNames   = ["def","extern"]
             }


int :: Parser Expr
int = do
    n <- integer lexer
    return $ Float (fromInteger n)


floating :: Parser Expr
floating = do
    n <- float lexer
    return $ Float n


expr :: Parser Expr
expr =
    let binary s f assoc = Infix (reservedOp lexer s >> return (BinOp f)) assoc
        table = [ [ binary "*" Times AssocLeft
                  , binary "/" Divide AssocLeft
                  ]
                , [ binary "+" Plus AssocLeft
                  , binary "-" Minus AssocLeft
                  ]
                ]

    in buildExpressionParser table factor


variable :: Parser Expr
variable = do
    var <- identifier lexer
    return $ Var var


function :: Parser Expr
function = do
    reserved lexer "def"
    name <- identifier lexer
    args <- parens lexer (many variable)
    body <- expr
    return $ Function name args body


extern :: Parser Expr
extern = do
    reserved lexer "extern"
    name <- identifier lexer
    args <- parens lexer (many variable)
    return $ Extern name args


call :: Parser Expr
call = do
    name <- identifier lexer
    args <- parens lexer (commaSep lexer expr)
    return $ Call name args


factor :: Parser Expr
factor = try floating
      <|> try int
      <|> try extern
      <|> try function
      <|> try call
      <|> variable
      <|> parens lexer expr


defn :: Parser Expr
defn = try extern
    <|> try function
    <|> expr


contents :: Parser a -> Parser a
contents p = do
    whiteSpace lexer
    r <- p
    eof
    return r


toplevel :: Parser [Expr]
toplevel = many $ do
    def <- defn
    reservedOp lexer ";"
    return def


parseExpr :: String -> Either ParseError Expr
parseExpr s = parse (contents expr) "<stdin>" s


parseToplevel :: String -> Either ParseError [Expr]
parseToplevel s = parse (contents toplevel) "<stdin>" s

