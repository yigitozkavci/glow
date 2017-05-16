module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Applicative ((<$>))

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Lexer
import Syntax

int :: Parser Expr
int = do
  n <- integer
  return $ Float (fromInteger n)

floating :: Parser Expr
floating = Float <$> float

character :: Parser Char
character = noneOf "\\\""

quotedCharacter :: Parser Expr
quotedCharacter = do
  char '\''
  chr <- character
  char '\''
  return $ Chr chr

quotedString :: Parser Expr
quotedString = do
  char '"'
  str <- string
  char '"'
  return $ Str str

string :: Parser String
string = many character

op :: Parser String
op = do
  whitespace
  o <- operator
  whitespace
  return o

binary s assoc = Ex.Infix (reservedOp s >> return (BinaryOp s)) assoc

binops = [ [binary "<-" Ex.AssocLeft
           ]
         , [ binary "*" Ex.AssocLeft
           , binary "/" Ex.AssocLeft
           ]
         , [ binary "+" Ex.AssocLeft
           , binary "-" Ex.AssocLeft
           ]
         , [ binary "<" Ex.AssocLeft
           , binary ">" Ex.AssocLeft
           ]
         ]

binop = Ex.Infix (BinaryOp <$> op) Ex.AssocLeft
unop = Ex.Prefix (UnaryOp <$> op)

expr :: Parser Expr
expr =  Ex.buildExpressionParser (binops ++ [[unop], [binop]]) factor

variable :: Parser Expr
variable = Var <$> identifier

function :: Parser Expr
function = do
  reserved "def"
  name <- identifier
  args <- parens $ commaSep identifier
  reservedOp "="
  body <- expr
  return $ Function name args body

extern :: Parser Expr
extern = do
  reserved "extern"
  name <- identifier
  args <- parens $ many identifier
  return $ Extern name args

ifthen :: Parser Expr
ifthen = do
  reserved "if"
  cond <- expr
  reserved "then"
  tr <- expr
  reserved "else"
  fl <- expr
  return $ If cond tr fl

for :: Parser Expr
for = do
  reserved "for"
  var <- identifier
  reservedOp "="
  start <- expr
  reservedOp ","
  cond <- expr
  reservedOp ","
  step <- expr
  reserved "in"
  body <- expr
  return $ For var start cond step body

call :: Parser Expr
call = do
  name <- identifier
  args <- parens $ commaSep expr
  return $ Call name args

factor :: Parser Expr
factor = try floating
      <|> try int
      <|> try quotedChar
      <|> try quotedString
      <|> try call
      <|> try variable
      <|> try ifthen
      <|> try letins
      <|> try for
      <|> parens expr

defn :: Parser Expr
defn = try extern
    <|> try function
    <|> try binaryDef
    <|> try unaryDef
    <|> expr

binaryDef :: Parser Expr
binaryDef = do
  reserved "def"
  reserved "binary"
  o <- op
  prec <- int
  args <- parens $ commaSep identifier
  reservedOp "="
  body <- expr
  return $ BinaryDef o args body

unaryDef :: Parser Expr
unaryDef = do
  reserved "def"
  reserved "unary"
  o <- op
  arg <- parens identifier
  reservedOp "="
  body <- expr
  return $ UnaryDef o arg body

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

toplevel :: Parser [Expr]
toplevel = many $ do
    def <- defn
    reservedOp ";"
    return def

letins :: Parser Expr
letins = do
  reserved "var"
  defs <- commaSep $ do
    var <- identifier
    reservedOp "<-"
    val <- expr
    return (var, val)
  reserved "in"
  body <- expr
  return $ foldr (uncurry Let) body defs

parseExpr :: String -> Either ParseError Expr
parseExpr s = parse (contents expr) "<stdin>" s

parseToplevel :: String -> Either ParseError [Expr]
parseToplevel s = parse (contents toplevel) "<stdin>" s
