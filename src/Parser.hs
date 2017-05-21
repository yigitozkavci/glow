module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Applicative ((<$>))

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Lexer
import Syntax
import Data.Word (Word32)

int :: Parser Expr
int = do
  n <- integer
  return $ Integer n

character :: Parser Expr
character = do
  char '\''
  c <- anyChar
  char '\''
  return $ Char c

-- Given a parser for single element, parses the list of it
array :: Parser a -> Parser [a]
array parseElem = do
  char '['
  whitespace
  vals <- commaSep parseElem
  whitespace
  char ']'
  whitespace
  return vals

intArray :: Parser Expr
intArray = IntArray <$> array integer

charArray :: Parser Expr
charArray = CharArray <$> array singleChar
  where singleChar :: Parser Char -- TODO: This is ugly, needs fix
        singleChar = do
          char '\''
          c <- anyChar
          char '\''
          return c

doubleArray :: Parser Expr
doubleArray = DoubleArray <$> array float

arrAccess :: Parser Expr
arrAccess = do
  var <- identifier
  char '['
  index <- integer
  char ']'
  return $ ArrAccess var (fromInteger index)

floating :: Parser Expr
floating = Float <$> float

op :: Parser String
op = do
  whitespace
  o <- operator
  whitespace
  return o

binary s assoc = Ex.Infix (reservedOp s >> return (BinaryOp s)) assoc

binops = [ [ binary "<-" Ex.AssocLeft
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

doubleDecl :: Parser TypeDecl
doubleDecl = do
  reserved "double"
  return DoubleDecl

intDecl :: Parser TypeDecl
intDecl = do
  reserved "int"
  return IntDecl

charDecl :: Parser TypeDecl
charDecl = do
  reserved "char"
  return CharDecl

doubleArrDecl :: Parser TypeDecl
doubleArrDecl = do
  reserved "double[]"
  return $ ArrayDecl DoublePrim

intArrDecl :: Parser TypeDecl
intArrDecl = do
  reserved "int[]"
  return $ ArrayDecl IntPrim

-- charArrDecl :: Parser TypeDecl
-- charArrDecl = do
--   reserved "char[]"
--   return $ ArrayDecl CharPrim

parseTypeDecl :: Parser TypeDecl
parseTypeDecl = try doubleDecl
             <|> try intDecl
             <|> try charDecl
             <|> try doubleArrDecl
             <|> intArrDecl

typedIdentifier :: Parser TypedName
typedIdentifier = do
  typeDecl <- parseTypeDecl
  whitespace
  var <- identifier
  return $ TypedName typeDecl var

function :: Parser Expr
function = do
  reserved "def"
  typeDecl <- parseTypeDecl
  name <- identifier
  args <- parens $ commaSep typedIdentifier
  reservedOp "="
  body <- expr
  return $ Function typeDecl name args body

extern :: Parser Expr
extern = do
  reserved "extern"
  typeDecl <- parseTypeDecl
  name <- identifier
  args <- parens $ commaSep typedIdentifier
  return $ Extern typeDecl name args

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
factor = try intArray
      <|> try charArray
      <|> try doubleArray
      <|> try floating
      <|> try int
      <|> try character
      <|> try arrAccess
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
  typeDecl <- parseTypeDecl
  reserved "binary"
  o <- op
  prec <- int
  args <- parens $ commaSep typedIdentifier
  reservedOp "="
  body <- expr
  return $ BinaryDef typeDecl o args body

unaryDef :: Parser Expr
unaryDef = do
  reserved "def"
  typeDecl <- parseTypeDecl
  reserved "unary"
  o <- op
  arg <- parens typedIdentifier
  reservedOp "="
  body <- expr
  return $ UnaryDef typeDecl o arg body

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
