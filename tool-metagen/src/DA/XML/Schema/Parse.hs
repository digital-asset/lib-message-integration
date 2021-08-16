-- Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | A Parser for the (approximated) XSD schema.
--
-- Note that we do not attempt to fully support the entire standard
-- XSD standard.
--
module DA.XML.Schema.Parse where

import Data.Char
import Data.List
import Data.List.Extra (split)
import Data.Map (Map)
import qualified Data.Map as Map
import Text.XML.Light (QName(..), Content(..), Element(..), CData(..))
import qualified Text.XML.Light         as XML
import DA.XML.Schema.Schema (Name)
import qualified DA.XML.Schema.Schema   as XSD

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Pos
import Text.Parsec.Error

-- import Debug.Trace
import Control.Monad
import qualified System.Directory as Dir
import System.Environment
import System.FilePath

-- | A Parser for XML 'Content'.
type XMLParser a = GenParser Content Namespaces a

-- | A Parser for text.
type TextParser a = GenParser Char Namespaces a

newtype Namespaces = Namespaces { unNamespaces :: Map Name XSD.AnyURI }
    deriving (Semigroup, Monoid)

xsdNS, xsiNS, xmlDsigNS :: String
xsdNS     = "http://www.w3.org/2001/XMLSchema"
xsiNS     = "http://www.w3.org/2001/XMLSchema-instance"
xmlDsigNS = "http://www.w3.org/2000/09/xmldsig#"



-- | Parse XSD schema using supplied name and string content.
parseXsd :: String -> String -> Either ParseError XSD.Schema
parseXsd name str = do
    xml <- maybe (Left err) Right $ XML.parseXMLDoc str
    runParser schema mempty name [XML.Elem xml]
  where
    err = newErrorMessage (Message "failed to parse XML content") (initialPos name)

-- | Parse a Schema declaration
schema :: XMLParser XSD.Schema
schema = do
    e <- xsdElement "schema"
    determineXsdNamespace e
    XSD.Schema
        <$> optionMaybe (attribute (mkQName "targetNamespace") uri e)
        <*> optionMaybe (attribute (mkQName "version")  anyString e)
        <*> optionMaybe (attribute (xsi "schemaLocation") anyString e)
        <*> optionMaybe (attribute (xsd "finalDefault") derivationSet e)
        <*> optionMaybe (attribute (xsd "blockDefault") derivationSet e)
        <*> option XSD.Unqualified (attribute (mkQName "elementFormDefault")   formChoice e)
        <*> option XSD.Unqualified (attribute (mkQName "attributeFormDefault") formChoice e)
        <*> recurseElems (many schemaItem) (elContent e)

schemaItem :: XMLParser XSD.SchemaItem
schemaItem = choice
    [ schemaItem_include
    , schemaItem_import
    , schemaItem_annotation
    , schemaItem_component
    ]

-- | Parse an <xsd:include>.
schemaItem_include :: XMLParser XSD.SchemaItem
schemaItem_include = do
    e <- xsdElement "include"
    fmap XSD.SchemaItem_Include $ XSD.Include
        <$> attribute (mkQName "schemaLocation") uri e
        <*> recurseWith (xsdTag "annotation") (optionMaybe annotation) (elContent e)

-- | Parse an <xsd:import>.
schemaItem_import :: XMLParser XSD.SchemaItem
schemaItem_import = do
    e <- xsdElement "import"
    fmap XSD.SchemaItem_Import $ XSD.Import
        <$> attribute (mkQName "namespace")      uri e
        <*> attribute (mkQName "schemaLocation") uri e
        <*> recurseWith (xsdTag "annotation") (optionMaybe annotation) (elContent e)

schemaItem_annotation :: XMLParser XSD.SchemaItem
schemaItem_annotation =
   XSD.SchemaItem_Annotation <$> annotation

-- | Parse an <xsd:annotation>.
annotation :: XMLParser XSD.Annotation
annotation = do
   annot <- xsdElement "annotation"
   (XSD.Annotation
        <$> recurseWith (xsdTag "documentation")
                        -- NB. FpML has empty annotation nodes (!)
                        -- which we represent as @Just ""@ to make parsing succeed.
                        (allChildren $ text <|> pure "")
                        (elContent annot))
       <|>
       -- If there is no documentation element, just include the text of the inner XML
       pure (XSD.Annotation $ XML.ppElement annot)

modelGroup :: XMLParser XSD.ModelGroup
modelGroup = do
    e <- xsdElement "group"
    pure (XSD.ModelGroup (Just $ XSD.Annotation "")) -- recurseWith (xsdTag "annotation") (optionMaybe annotation) (elContent e)
        <*> nameOrRef e
        <*> occurs e
        <*> recurseElemsWith (not . xsdTag "annotation") (optionMaybe elements) (elContent e)

elements :: XMLParser XSD.Elements
elements = choice
    [ elements_choice
    , elements_sequence
    , elements_element
    , elements_group
    , elements_any
    ]
  where
    elements_choice = do
        e <- xsdElement "choice"
        XSD.Elements_Choice
            <$> recurseWith (xsdTag "annotation") (optionMaybe annotation) (elContent e)
            <*> occurs e
            <*> recurseElemsWith (not . xsdTag "annotation") (many elements) (elContent e)

    elements_sequence = do
        e <- xsdElement "sequence"
        XSD.Elements_Sequence
            <$> recurseWith (xsdTag "annotation") (optionMaybe annotation) (elContent e)
            <*> occurs e
            <*> recurseElemsWith (not . xsdTag "annotation") (many elements) (elContent e)

    elements_element =
        XSD.Elements_Element <$> elementDefn

    elements_group =
        XSD.Elements_Group <$> modelGroup

    elements_any =
        XSD.Elements_Any <$ xsdElement "any"

schemaItem_component :: XMLParser XSD.SchemaItem
schemaItem_component =
    XSD.SchemaItem_Component
        <$> choice
        [ XSD.Component_Attribute      <$> attributeDefn
        , XSD.Component_Element        <$> elementDefn
        , XSD.Component_SimpleType     <$> simpleType
        , XSD.Component_ComplexType    <$> complexType
        , XSD.Component_ModelGroup     <$> modelGroup
        , XSD.Component_AttributeGroup <$> attributeGroup
        --    , Component_AnySimpleType
        --    , Component_AnyType
        --    , Component_AnyElem
        ]

attributeDefn :: XMLParser XSD.Attribute
attributeDefn = do
    e    <- xsdElement "attribute"
    XSD.Attribute
        <$> nameWithTypeOrRef e
        <*> option XSD.Optional (attribute (mkQName "use") use e)
        <*> optionMaybe (attribute (mkQName "default") anyString e)
        <*> optionMaybe (attribute (mkQName "fixed")   anyString e)
        <*> option XSD.Unqualified (attribute (mkQName "form") formChoice e)
        <*> optionMaybe (attribute (mkQName "targetNamespace") uri e)
        <*> optionMaybe (attribute (mkQName "inheritable") bool e)
        <*> recurseWith (xsdTag "annotation") (optionMaybe annotation) (elContent e)
        <*> recurseWith (xsdTag "simpleType") (optionMaybe simpleType) (elContent e)

elementDefn :: XMLParser XSD.Element
elementDefn = do
    e    <- xsdElement "element"
    XSD.Element
        <$> nameWithTypeOrRef e
        <*> occurs e
        <*> optionMaybe (attribute (mkQName "default") anyString e)
        <*> optionMaybe (attribute (mkQName "fixed")   anyString e)
        <*> option False (attribute (mkQName "nillable") bool e)
        <*> option False (attribute (mkQName "abstract") bool e)
        <*> optionMaybe (attribute (mkQName "substitutionGroup") qname e)
        <*> optionMaybe (attribute (mkQName "final") derivationSet e)
        <*> optionMaybe (attribute (mkQName "block") derivationSet e)
        <*> option XSD.Unqualified (attribute (mkQName "form") formChoice e)
        <*> recurseWith (xsdTag "annotation") (optionMaybe annotation) (elContent e)
        <*> recurseElemsWith (not . xsdTag "annotation") (optionMaybe $ simpleType `eitherP` complexType) (elContent e)

-- | Parse a <xsd:simpleType> definition.
simpleType :: XMLParser XSD.SimpleType
simpleType = do
    e    <- xsdElement "simpleType"
    XSD.SimpleType
        <$> recurseWith (xsdTag "annotation") (optionMaybe annotation) (elContent e)
        <*> optionMaybe (attribute (mkQName "name") anyString e)
        <*> optionMaybe (attribute (mkQName "final") derivationSet e)
        <*> recurseElemsWith (not . xsdTag "annotation") simpleItem (elContent e)
  where
      simpleItem = choice
          [ restricted, listOf, unionOf]
      restricted =
          XSD.Restricted <$> simpleRestriction
      listOf = do
          e  <- xsdElement "list"
          XSD.ListOf <$> attribute (mkQName "itemType") qname e
                     `eitherP` recurseWith (xsdTag "simpleType") simpleType (elContent e)
      unionOf  = do
          e  <- xsdElement "union"
          XSD.UnionOf
              <$> optionalAttribute (mkQName "memberTypes") (many qname) e
              <*> recurseWith (xsdTag "simpleType") (many simpleType) (elContent e)

restriction :: XMLParser t -> XMLParser (XSD.Restriction t)
restriction p = do
    e <- xsdElement "restriction"
    XSD.Restriction
        <$> recurseWith (xsdTag "annotation") (optionMaybe annotation) (elContent e)
        <*> optionMaybe (attribute (mkQName "base") qname e)
        <*> recurseElemsWith (not . xsdTag "annotation") p (elContent e)

simpleRestriction :: XMLParser XSD.SimpleRestriction
simpleRestriction =
    restriction $ (,) <$> optionMaybe simpleType <*> many facet

extension :: XMLParser t -> XMLParser (XSD.Extension t)
extension p = do
    e <- xsdElement "extension"
    XSD.Extension
        <$> recurseWith (xsdTag "annotation") (optionMaybe annotation) (elContent e)
        <*> optionMaybe (attribute (mkQName "base") qname e)
        <*> recurseElemsWith (not . xsdTag "annotation") (optionMaybe p) (elContent e)

attributes :: XMLParser XSD.Attributes
attributes = many1 attributeDefn `eitherP` attributeGroup

nameOrRef :: Element -> XMLParser (Either Name QName)
nameOrRef e =
    attribute (mkQName "name") anyString e
    `eitherP`
    attribute (mkQName "ref") qname {-qname q-} e

nameWithTypeOrRef :: Element -> XMLParser (Either XSD.NameWithType QName)
nameWithTypeOrRef e =
    nameWithType e `eitherP` attribute (mkQName "ref") qname {-qname q-} e

attributeGroup :: XMLParser XSD.AttributeGroup
attributeGroup = do
    e <- xsdElement "attributeGroup"
    XSD.AttributeGroup
        <$> recurseWith (xsdTag "annotation") (optionMaybe annotation) (elContent e)
        <*> nameOrRef e
        <*> recurseElemsWith (not . xsdTag "annotation") (many attributes) (elContent e)

-- | Parse a <xsd:complexType> definition.
complexType :: XMLParser XSD.ComplexType
complexType = do
    e <- xsdElement "complexType"
    XSD.ComplexType
        <$> recurseWith (xsdTag "annotation") (optionMaybe annotation) (elContent e)
        <*> optionMaybe (attribute (mkQName "name") anyString e)
        <*> option False (attribute (mkQName "abstract") bool e)
        <*> optionMaybe (attribute (mkQName "final") derivationSet e)
        <*> optionMaybe (attribute (mkQName "block") derivationSet e)
        <*> option False (attribute (mkQName "mixed") bool e)
        <*> recurseElemsWith (not . xsdTag "annotation") (optionMaybe complexItem) (elContent e)

complexItem :: XMLParser XSD.ComplexItem
complexItem =
    complexContent <|> simpleContent <|> otherContent
  where
    -- TODO do these have annotations?
    complexContent = do
        e     <- xsdElement "complexContent"
        XSD.ComplexContent
            <$> recurseElemsWith (not . xsdTag "annotation") complexContent' (elContent e)
            <*> option False (attribute (mkQName "mixed") bool e)

    simpleContent = do
        e <- xsdElement "simpleContent"
        XSD.SimpleContent
            <$> recurseElemsWith (not . xsdTag "annotation") simpleContent' (elContent e)
    otherContent =
        XSD.OtherContent <$> otherContent'

    complexContent' =
        restriction otherContent' `eitherP` extension otherContent'

    simpleContent' =
       simpleRestriction `eitherP` extension attributes

    otherContent' =
       (,,) <$> optionMaybe elements
            <*> many attributes
            <*> optionMaybe anyAttr


-- | Parse an <xsd:anyAttribute>
anyAttr :: XMLParser XSD.AnyAttr
anyAttr = do
    e <- xsdElement "anyAttribute"
    XSD.AnyAttr
        <$> recurseWith (xsdTag "annotation") (optionMaybe annotation) (elContent e)
        <*> attribute (mkQName "namespace") uri e

-- | Parse name and type attributes.
nameWithType :: {-(String->String->QName) ->-} Element -> XMLParser XSD.NameWithType
nameWithType e =
    XSD.NameWithType
        <$> attribute   (mkQName "name") anyString e
        <*> optionMaybe (attribute (mkQName "type") qname e) -- (qname q) e)

occurs :: Element -> XMLParser XSD.Occurs
occurs e =
  XSD.Occurs
        <$> option 1 (attribute (mkQName "minOccurs") decimal e)
        <*> option (XSD.Bounded 1) (attribute (mkQName "maxOccurs") bounded e)
  where
    bounded = (XSD.Bounded <$> decimal)
          <|> (XSD.Unbounded <$ string "unbounded")

facet :: XMLParser XSD.Facet
facet = choice
    [ mkFacet XSD.Facet_MinInclusive    "minInclusive"
    , mkFacet XSD.Facet_MinExclusive    "minExclusive"
    , mkFacet XSD.Facet_MaxInclusive    "maxInclusive"
    , mkFacet XSD.Facet_MaxExclusive    "maxExclusive"
    , mkFacet XSD.Facet_TotalDigits     "totalDigits"
    , mkFacet XSD.Facet_FractionDigits  "fractionDigits"
    , mkFacet XSD.Facet_Length          "length"
    , mkFacet XSD.Facet_MinLength       "minLength"
    , mkFacet XSD.Facet_MaxLength       "maxLength"
    , mkFacet XSD.Facet_Enumeration     "enumeration"
    , mkFacet XSD.Facet_Whitespace      "whiteSpace"
    , mkFacet XSD.Facet_Pattern         "pattern"
    ]
  where
    mkFacet kind tag = do
        e <- xsdElement tag
        XSD.Facet
            <$> recurseWith (xsdTag "annotation") (optionMaybe annotation) (elContent e)
            <*> pure kind
            <*> attribute (mkQName "value") anyString e
            <*> option False (attribute (mkQName "fixed") bool e)

formChoice :: TextParser XSD.FormChoice
formChoice = do
    w <- many1 lower
    case w of
        "qualified"   -> return XSD.Qualified
        "unqualified" -> return XSD.Unqualified
        _             -> fail "Expected \"qualified\" or \"unqualified\""

derivationSet :: TextParser XSD.DerivationSet
derivationSet = do
    w <- many1 (lower <|> char '#')
    case w of
        "restriction" -> return XSD.NoRestriction
        "extension"   -> return XSD.NoExtension
        "#all"        -> return XSD.AllFinal
        _             -> fail $ "Expected \"restriction\" or \"extension\""
                                   ++" or \"#all\""
use :: TextParser XSD.Use
use = do
    w <- many1 letter
    case w of
        "required"   -> return XSD.Required
        "optional"   -> return XSD.Optional
        "prohibited" -> return XSD.Prohibited
        _            -> fail "Could not parse \"use\" attribute value"

-- | The next content element checking that the supplied name matches and
-- belongs to the XSD namespace.
xsdElement :: Name -> XMLParser Element
xsdElement n = do
    elementWith (xsdTag n) ("xsd element " <> n)

-- | Qualify an ordinary name with the XSD namespace.
-- NOTE: equality on QName's is done by considering the URL instead of
-- the prefix, if the URL is set.
xsd :: Name -> QName
xsd name =
    QName {qName=name,qURI=Just xsdNS,qPrefix=Just "xsd"}

-- | Qualify an ordinary name with the XSI namespace.
xsi :: Name -> QName
xsi name = QName {qName=name,qURI=Just xsiNS,qPrefix=Just "xsi"}

-- | Predicate for comparing against an XSD-qualified name.
-- NOTE: the xml library sets the URL on the qualified name according to the
-- xmlns attribute on the top-level element and uses this for QName equality.
xsdTag :: String -> Content -> Bool
xsdTag tag (Elem (Element qn _ _ _)) = qn == xsd tag
xsdTag _   _                         = False


------------------------------------------------------------
-- XML parsing kit

-- Note: Unfortunately the XML library "xml", gives us very poor positional
-- information.
getPos :: Content -> SourcePos
getPos cont = newPos "" (maybe 1 fromInteger $ pos cont) 1
  where
    pos (Elem Element{elLine}) = elLine  -- Just a line number
    pos (Text CData{cdLine})   = cdLine  -- Just a line number
    pos CRef{}                 = Nothing -- Not even a line number available :(

-- | Accepts content verified by a function.
content :: (Content -> Maybe a) -> XMLParser a
content = token show getPos

-- | The next content element, checking that the supplied predicate matches.
elementWith :: (Content -> Bool) -> String -> XMLParser Element
elementWith match msg = content (\case
    c@(Elem el) | match c -> return el
    _ -> Nothing)
    <?> msg

-- | The next content element, whatever it may be.
anyElement :: XMLParser Element
anyElement = elementWith (const True) "any element"

-- | Grabs all plain text from the next token.
text :: XMLParser String
text = content (\case
    Elem el              -> Just $ XML.strContent el
    Text (CData _ str _) -> Just str
    _ -> Nothing)
    <?> "text"

-- | Helper function to recurse through an XML document.
recurse :: XMLParser a -> [Content] -> XMLParser a
recurse p conts = do
    inp <- getInput
    setInput conts
    res <- p
    setInput inp
    return res

recurseWith :: (Content -> Bool) -> XMLParser a -> [Content] -> XMLParser a
recurseWith keep p conts =
    recurse p $ filter keep conts

recurseElemsWith :: (Content -> Bool) -> XMLParser a -> [Content] -> XMLParser a
recurseElemsWith keep p conts =
    recurse p [ c | c@Elem{} <- conts, keep c ]

recurseElems :: XMLParser a -> [Content] -> XMLParser a
recurseElems = recurseElemsWith (const True)

-- | Grab and parse any and all children of the next element.
allChildren :: XMLParser a -> XMLParser a
allChildren p = do
    conts <- elContent <$> anyElement
    recurse p conts

-- | Lookup an attribute in the given element, fail if it doesn't exist.
-- NOTE: equality on QName's is done by considering the URL instead of
-- the prefix, if the URL is set.
attribute :: QName -> TextParser a -> Element -> XMLParser a
attribute qn p (Element n as _ _) =
    case XML.lookupAttr qn as of
         Nothing  -> fail $ "attribute "++show qn
                          ++" not present in <"++show n++">"
         Just av  -> do
             namespaces <- getState
             either (fail . show) return $ runParser p namespaces (show qn) av

optionalAttribute :: Monoid a => QName -> TextParser a -> Element -> XMLParser a
optionalAttribute qn p (Element _ as _ _) =
    case XML.lookupAttr qn as of
         Nothing  -> mempty
         Just av  -> do
             namespaces <- getState
             either (fail . show) return $ runParser p namespaces (show qn) av

-- | Parse an XML textual boolean, i.e. "true", "false", "0", or "1"
bool :: TextParser Bool
bool = do w <- many1 alphaNum
          case w of
            "true"  -> return True
            "false" -> return False
            "0"     -> return True
            "1"     -> return False
            _       -> fail "Could not parse boolean value"

anyString :: TextParser String
anyString = many1 anyChar

uri :: TextParser XSD.AnyURI
uri = many1 anyChar

decimal :: TextParser Int
decimal =
    foldl' (\a i -> a * 10 + digitToInt i) 0 <$> many1 digit

eitherP :: GenParser t u a -> GenParser t u b -> GenParser t u (Either a b)
eitherP p q = Left <$> p <|> Right <$> q

-- NOTE URI is ignored, unless it is the XSD namespace
qname :: TextParser QName
qname = do
    ns  <- unNamespaces <$> getState
    str <- many1 (satisfy (not . isSpace)) <* spaces
    case split (==':') str of
        [n]    -> return QName{qName=n,qURI=Nothing,qPrefix=Nothing}
        [p, n] -> return QName{qName=n,qURI=Map.lookup p ns,qPrefix=Just p}
        _      -> mzero


mkQName :: Name -> QName
mkQName name = QName {qName=name,qURI=Nothing,qPrefix=Nothing}

-- | Currently we just track the XSD prefix
determineXsdNamespace :: Element -> XMLParser Namespaces
determineXsdNamespace e =
    let xmlns = filter (\a -> qPrefix (XML.attrKey a) == Just "xmlns") (elAttribs e)
    in case find (\a -> XML.attrVal a == xsdNS) xmlns of
         Nothing -> fail "No XSD namespace defined in schema element."
         Just a  -> do
             let prefix = qName (XML.attrKey a)
                 ns = Namespaces $ Map.singleton prefix (XML.attrVal a)
             setState ns
             return ns


------------------------------------------------------------
-- Testing

main :: IO ()
main = do
    baseDir <- (</> "solutions/finance-integration/src/main/resources/fpml/confirmation")
               <$> getEnv "DADE_REPO_ROOT"

    files <- filter isXSD <$> Dir.listDirectory baseDir
    forM_ files $ \file -> do
        str <- readFile $ baseDir </> file
        case parseXsd file str of
            -- Right _s  -> putStrLn $ file ++ " : PASS"
            Right s  -> print s
            Left  err -> do
                putStrLn $ file ++ " : FAIL"
                error $ show err
  where
    isXSD f = takeExtension f == ".xsd"
