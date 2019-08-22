{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- | This module is an attempt to reverse engineer and parse the undocumented and very
-- adhoc "Rosetta" language.
--
-- Problems with the language that make it difficult parse:
-- * Mixture of whitespace/indentation sensitive syntax (with tabs) and curly-brace syntax.
-- * Context sensitive grammar, e.g. the meaning of "id" depends on context
-- * No documented formal grammar

module DA.CDM.Rosetta.Parse where

import Control.Applicative
import qualified DA.CDM.Rosetta.Schema as Rosetta

import Text.Parsec ( many1, choice, skipMany, skipMany1, option
                   , optionMaybe, oneOf, noneOf )
import Text.Parsec.Indent
import Text.Parsec.Indent.Explicit
import Text.Parsec.Indent.Internal
import qualified Text.Parsec as P
import qualified Text.Parsec.Token as P
import qualified Text.Parsec.Language as P

--import Debug.Trace
import Control.Monad
import Control.Monad.Identity

import qualified Data.List as List
import qualified Data.Set as Set

import qualified System.Directory as Dir
import System.Environment
import System.FilePath

-- NOTE: CDM Rosetta has a mixture of whitespace agnostic syntax (semicolon delimited) and
-- tab-delimited syntax (!)
type Parser a = IndentParser String () a

-- | Parse CDM Rosetta schema using supplied name and string content.
parseRosetta :: String -> String -> Either P.ParseError Rosetta.Schema
parseRosetta name str = runIndentParser (rosettaSchema <* P.eof) () name str

-- | Parse a Rosetta file unit.
rosettaSchema :: Parser Rosetta.Schema
rosettaSchema =
    Rosetta.Schema
        <$> namespace
        <*> version
        <*> many1 rosettaDecl

rosettaDecl :: Parser Rosetta.Decl
rosettaDecl = choice
    [ Rosetta.ClassDecl       <$> rosettaClass
    , Rosetta.EnumDecl        <$> rosettaEnum
    , Rosetta.DataRuleDecl    <$> rosettaDataRule
    , Rosetta.ChoiceRuleDecl  <$> rosettaChoiceRule
    , Rosetta.IsEventDecl     <$> rosettaIsEvent
    , Rosetta.IsProductDecl   <$> rosettaIsProduct
    , Rosetta.AliasDecl       <$> rosettaAlias
    ]

rosettaClass :: Parser Rosetta.Class
rosettaClass =
    Rosetta.Class
        <$> option False (True <$ reserved "abstract")
        <*> (reserved "class" *> upperIdentifier)
        <*> optionMaybe (reserved "extends" *> upperIdentifier)
        <*> (optional calculation *> (Set.fromList <$> P.sepBy classMeta whiteSpace))
        <*> optionMaybe annotation <* skipMany synonym
        <*> braces (many (classField <* optionMaybe semi <* skipMany synonym)) -- NB: some classes have no fields

rosettaEnum :: Parser Rosetta.Enum
rosettaEnum =
    Rosetta.Enum
        <$> (reserved "enum" *> upperIdentifier)
        <*> optionMaybe (reserved "extends" *> upperIdentifier)
        <*> (optionMaybe annotation <* skipMany synonym)
        <*> braces (P.sepBy1 (enumField <* skipMany synonym) comma)

-- SKIPPED
rosettaDataRule :: Parser Rosetta.DataRule
rosettaDataRule =
    Rosetta.DataRule
        <$> (reserved "data" *> reserved "rule" *> upperIdentifier)
        <*> (optionMaybe annotation <* block' skipLine)

-- SKIPPED
rosettaChoiceRule :: Parser Rosetta.ChoiceRule
rosettaChoiceRule =
    Rosetta.ChoiceRule
        <$> (reserved "choice" *> reserved "rule" *> upperIdentifier)
        <*> (optionMaybe annotation <* block' skipLine)

-- SKIPPED
rosettaIsEvent :: Parser Rosetta.IsEvent
rosettaIsEvent =
    Rosetta.IsEvent
        <$> (reserved "isEvent" *> upperIdentifier)
        <*> (optionMaybe annotation <* block' skipLine)

-- SKIPPED
rosettaIsProduct :: Parser Rosetta.IsProduct
rosettaIsProduct =
    Rosetta.IsProduct
        <$> (reserved "isProduct" *> upperIdentifier)
        <*> (optionMaybe annotation <* block' skipLine)

-- SKIPPED
rosettaAlias :: Parser Rosetta.Alias
rosettaAlias =
    Rosetta.Alias
        <$> (reserved "alias" *> identifier)
        <*> (optionMaybe annotation <* block' skipLine)

skipLine :: Parser ()
skipLine = () <$ (skipMany1 $ noneOf "\r\n") <* (optional P.endOfLine <* whiteSpace)

classField :: Parser Rosetta.ClassField
classField =
    Rosetta.ClassField
        <$> lowerIdentifier
        <*> optionMaybe identifier
        <*> cardinality
        <*> (Set.fromList <$> (P.sepBy fieldMeta comma))
        <*> optionMaybe annotation

enumField :: Parser Rosetta.EnumField
enumField =
    Rosetta.EnumField
        <$> identifier -- NB: some start with lowercase letters or symbols
        <*> optionMaybe ((P.string "displayName" <* whiteSpace) *> stringLiteral)
        <*> optionMaybe style
        <*> optionMaybe annotation <* skipMany synonym

namespace :: Parser Rosetta.Namespace
namespace =
    Rosetta.Namespace <$> (reserved "namespace" *> stringLiteral)

version :: Parser Rosetta.Version
version =
    Rosetta.Version <$> (reserved "version" *> stringLiteral)

-- we later skip the dubious calculation "keyword"
calculation :: Parser [Rosetta.Identifier]
calculation = (P.string "calculation" <* whiteSpace) *> P.sepBy1 identifier comma

style :: Parser Rosetta.Style
style = reserved "style" *> (Rosetta.Style <$> identifier)

classMeta :: Parser Rosetta.ClassMeta
classMeta =
    Rosetta.COneOf <$ (P.string "one of" <* whiteSpace) <|>
    Rosetta.CKey <$ reserved "key" <|>
    Rosetta.CRosettaKeyValue <$ reserved "rosettaKeyValue"

fieldMeta :: Parser Rosetta.FieldMeta
fieldMeta =
    Rosetta.FReference <$ reserved "reference" <|>
    Rosetta.FScheme <$ reserved "scheme" <|>
    Rosetta.FScheme <$ reserved "id"

cardinality :: Parser Rosetta.Cardinality
cardinality = parens $
    Rosetta.Cardinality
        <$> integer
        <*> (P.string ".." *> bound)

bound :: Parser Rosetta.Bound
bound =
    (Rosetta.Unbounded <$ P.char '*') <|>
    (Rosetta.Bounded <$> integer)

-- skip these for now
synonym :: Parser ()
synonym = () <$ brackets (reserved "synonym" *> skipMany1 (noneOf "]"))

annotation :: Parser Rosetta.Annotation
annotation = Rosetta.Annotation <$> angles stringLiteral

-- | Parses a block of lines at the same or greater indentation level
-- starting at the current position.
block'
    :: (Monad m, P.Stream s m z)
    => P.ParsecT s u m a
    -> P.ParsecT s u m [a]
block' p = do
    ref <- indentation
    many1 (indented' ref >> p)

-- | Parses only when indented equal or past the level of the reference
indented'
    :: (Monad m, P.Stream s m z)
    => Indentation  -- ^ Reference indentation
    -> P.ParsecT s u m ()
indented' ref = do
    pos <- indentation
    when (iColumn pos < iColumn ref) $ P.unexpected (prettyIndentation pos)

------------------------------------------------------------
-- Language definition for Lexer

languageDef :: P.GenLanguageDef String () (IndentT Identity)
languageDef = P.emptyDef
    { P.commentStart   = "/*"
    , P.commentEnd     = "*/"
    , P.commentLine    = "//"
    , P.nestedComments = True
    , P.identStart     = P.letter <|> oneOf "_"
    , P.identLetter    = P.alphaNum <|> oneOf "_'"
    , P.opStart        = P.opLetter languageDef
    , P.opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
    , P.reservedOpNames= []
    , P.reservedNames  = [ "namespace", "class", "enum", "data", "choice", "rule", "extends"
                         , "rosettaKey", "rossetaKeyValue", "scheme", "anchor", "reference"
                         , "abstract", "alias", "isEvent", "isProduct", "style"
                         ]
    , P.caseSensitive  = True
    }

------------------------------------------------------------
-- Lexer

lexer = P.makeTokenParser languageDef

lowerIdentifier = P.lookAhead P.lower >> identifier
upperIdentifier = P.lookAhead P.upper >> identifier

identifier = Rosetta.Identifier <$> P.identifier lexer

reserved = P.reserved lexer
operator = P.operator lexer
reservedOp = P.reservedOp lexer
charLiteral = P.charLiteral lexer
stringLiteral = P.stringLiteral lexer
--natural = P.natural lexer
integer = P.integer lexer
--float = P.float lexer
naturalOrFloat = P.naturalOrFloat lexer
--decimal = P.decimal lexer
--hexadecimal = P.hexadecimal lexer
--octal = P.octal lexer
symbol = P.symbol lexer
lexeme = P.lexeme lexer
whiteSpace = P.whiteSpace lexer
parens = P.parens lexer
braces = P.braces lexer
angles = P.angles lexer
brackets = P.brackets lexer
semi = P.semi lexer
comma = P.comma lexer
colon = P.colon lexer
dot = P.dot lexer
semiSep = P.semiSep lexer
semiSep1 = P.semiSep1 lexer
commaSep = P.commaSep lexer
commaSep1 = P.commaSep1 lexer


------------------------------------------------------------
-- Testing

main :: IO ()
main = do
    baseDir <- (</> "solutions/cdm/cdm-distribution-1.1.107/common-domain-model")
               <$> getEnv "DADE_REPO_ROOT"
    files  <- filter isRosetta <$> Dir.listDirectory baseDir
    let blacklist = ["model-cdm-config.rosetta", "model-cdm-calculations.rosetta"]
    forM_ (files List.\\ blacklist) $ \file -> do
        str <- readFile $ baseDir </> file
        case parseRosetta file str of
            Right _s  -> putStrLn $ file ++ " : PASS"
            --Right s  -> putStrLn $ show s
            Left  err -> do
                putStrLn $ file ++ " : FAIL : " ++ show err
--                error $ show err
  where
    isRosetta f = takeExtension f == ".rosetta"
