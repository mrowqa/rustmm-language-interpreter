{-# Options -Wall #-}

module Interpreter.Parser (
    parseCode,
    ParseResult
) where
import Control.Monad (void)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
-- import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char.Lexer as L

import Interpreter.Defs
import Interpreter.Primitives


-- based on following tutorial:
-- https://markkarpov.com/megaparsec/parsing-simple-imperative-language.html
type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

-- | 'parens' parses something between parenthesis.
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | 'integer' parses an integer.
integer :: Parser Integer
integer = lexeme L.decimal

identifierChar :: Parser Char
identifierChar = char '_' <|> alphaNumChar

rword :: String -> Parser ()
rword w = (lexeme . try) (string w *> notFollowedBy identifierChar)

rws :: [String] -- list of reserved words
rws = ["let","mut","if","else","while","true","false","fn","call","int","bool"] -- TODO more?

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many identifierChar
    check x = if x `elem` rws
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x
-- </basedOn>

topDef :: Parser Exp
topDef = sFnDef

sExp :: Parser Exp
sExp = do
      topDef
  <|> sExpLet
  <|> eExp
  <|> (pure $ ELitVal VUnit)

eExp :: Parser Exp
eExp = do
      eLiteral
--  <|> eIf
--  <|> eWhile
  <|> eFnCall
  <|> parens eExp

--eExp :: Parser Exp
--eExp = makeExprParser eTerm hOperators

-- eTerm :: Parser Exp
-- eTerm = do
--       eLiteral
--   <|> eIf
--   <|> eWhile
--   <|> eFnCall
--   <|> parens eExp

-- hOperators :: [[Operator Parser Exp]]
-- hOperators = [[
--         Prefix (bltnNeg <$ symbol "-")
--     ], [
--         InfixL (bltnMul <$ symbol "*"),
--         InfixL (bltnDiv <$ symbol "/")
--     ], [
--         InfixL (bltnAdd <$ symbol "+"),
--         InfixL (bltnSub <$ symbol "-")
--     -- TODO
--     -- relations <=, <, ==, etc
--     -- logic && ||
--     ]]

sExpLet :: Parser Exp
sExpLet = do
    rword "let"
    mut <- optMut
    var <- identifier
    void $ symbol "="
    exp' <- eExp
    return $ ELet mut var exp' emptyBlock -- continuation/subexpr is shifted in block

optMut :: Parser Bool
optMut = try (rword "mut" *> pure True) <|> pure False

eLiteral :: Parser Exp
eLiteral = do
      eInteger
  <|> eBool
  <|> try eUnit
  <|> eVar
  <|> eTakeRef
  <|> eAssign
  <|> eClosure

eInteger :: Parser Exp
eInteger = do
    val <- integer
    return $ ELitVal $ VInt val

eBool :: Parser Exp
eBool = do
    val <- try (rword "true" *> pure True) <|> (rword "false" *> pure False)
    return $ ELitVal $ VBool val

eUnit :: Parser Exp
eUnit = do
    parens $ pure ()
    return $ ELitVal $ VUnit

-- eIf :: Parser Exp
-- eIf = do
--     rword "if"
--     cond <- eExp
--     brTrue <- eBlock
--     brFalse <- try (rword "else" *> eBlock) <|> return emptyBlock
--     return $ EFnCall bltnIf [cond, pack brTrue, pack brFalse]
--   where pack :: Exp -> Exp
--         pack body = ELitVal $ VFn t1 [] t2

-- eWhile :: Parser Exp
-- eWhile = undefined

eFnCall :: Parser Exp
eFnCall = do
    rword "call" -- TODO how to write the grammar without left recursion?
    -- alternative: require call via variable
    fn <- eExp
    args <- parens $ eExp `sepBy` symbol ","
    return $ EFnCall fn args

eVar :: Parser Exp
eVar = do
    var <- identifier
    return $ EVar var

eTakeRef :: Parser Exp
eTakeRef = do
    void $ symbol "&"
    mut <- optMut
    var <- identifier
    return $ ETakeRef mut var

eClosure :: Parser Exp
eClosure = do
    let walled = between (symbol "|") (symbol "|")
    args <- walled hFnDefArgs
    let (names, types) = unzip args
    -- version with return type inference - not supported
    retType <- try (symbol "->" *> hType) <|> pure TUnit
    block <- eBlock
    return $ ELitVal $ VFn (TFn types retType) names block

eAssign :: Parser Exp
eAssign = do
    var <- identifier
    void $ symbol "="
    exp' <- eExp
    return $ EAssign var exp'

sFnDef :: Parser Exp
sFnDef = do
    rword "fn"
    mut <- optMut
    name <- identifier
    args <- parens hFnDefArgs
    let (names, types) = unzip args -- TODO args can be mutable...
    retType <- try (symbol "->" *> hType) <|> pure TUnit
    block <- eBlock
    let fn = ELitVal $ VFn (TFn types retType) names block
    return $ ELet mut name fn emptyBlock

hFnDefArgs :: Parser [(Var, Type)]
hFnDefArgs = hTypedArg `sepBy` symbol ","

hTypedArg :: Parser (Var, Type)
hTypedArg = do
    var <- identifier
    void $ symbol ":"
    typ <- hType
    return $ (var, typ)

hType :: Parser Type
hType = do
      rword "int" *> return TInt
  <|> rword "bool" *> return TBool
  <|> parens (pure ()) *> return TUnit
  <|> hFnType
  <|> hRefType

hFnType :: Parser Type
hFnType = do
    rword "fn"
    argsTypes <- parens $ hType `sepBy` symbol ","
    retType <- try (symbol "->" *> hType) <|> pure TUnit
    return $ TFn argsTypes retType

hRefType :: Parser Type
hRefType = do
    void $ symbol "&"
    mut <- optMut
    subtype <- hType
    return $ TRef mut subtype

eBlock :: Parser Exp
eBlock = do
    let embraced = between (symbol "{") (symbol "}") -- funny, isn't it?
    exps <- embraced $ sExp `sepBy` symbol ";"
    return $ EBlock $ shiftLets exps []
  where shiftLets :: [Exp] -> [Exp] -> [Exp]
        shiftLets [] acc = reverse acc
        shiftLets (x:xs) acc = case x of
            ELet mut name val _ -> reverse $ (ELet mut name val (EBlock $ shiftLets xs [])):acc
            _ -> shiftLets xs (x:acc)


langParser :: Parser Exp
langParser = do
    sc
    body <- many topDef
    return (EBlock body) <* eof


type ParseResult = Either (ParseError (Token String) Void) Exp

parseCode :: String -> String -> ParseResult
parseCode = parse langParser

-- TODO positions!
-- TODO expressions (operators!)
-- TODO if, while
-- TODO fn call -> mut arg

-- TODO typecheck
-- TODO interpreter :(
