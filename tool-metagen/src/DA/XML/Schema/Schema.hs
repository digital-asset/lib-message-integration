-- Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

-- |
-- XSD Schema is a horrible overly complex and adhoc standard which I have only approximated
-- here. Currently, we support just enough of XSD in order to support FpML.
--
module DA.XML.Schema.Schema where

import Text.XML.Light (QName)

type Name    = String
type AnyURI  = String
type ID      = String

newtype Annotation = Annotation { unAnnotation :: String }
    deriving (Eq,Show)

instance Semigroup Annotation where
    (<>) (Annotation a1) (Annotation a2) = Annotation $ a1 <> "\n" <> a2

data Schema = Schema
    { schema_targetNamespace       :: Maybe AnyURI
    , schema_version               :: Maybe String
    , schema_schemaLocation        :: Maybe String
    , schema_finalDefault          :: Maybe DerivationSet
    , schema_blockDefault          :: Maybe DerivationSet
    , schema_attributeFormDefault  :: FormChoice
    , schema_elementFormDefault    :: FormChoice
    , schema_items                 :: [SchemaItem]
    }
    deriving (Eq,Show)

data Include = Include
    { include_schemaLocation :: AnyURI
    , include_annotation     :: Maybe Annotation
    }
    deriving (Eq,Show)

data Import = Import
    { import_namespace      :: AnyURI
    , import_schemaLocation :: AnyURI
    , import_annotation     :: Maybe Annotation
    }
    deriving (Eq,Show)

data SchemaItem
    = SchemaItem_Include      Include
    | SchemaItem_Import       Import
    | SchemaItem_Annotation   Annotation
--  | SchemaItem_Redefine     Redefine
    | SchemaItem_Component    Component
    deriving (Eq,Show)

data Use = Required | Optional | Prohibited
    deriving (Eq,Show)

data Occurs = Occurs
    { occurs_minimum :: Int
    , occurs_maximum :: Bound
    }
    deriving (Eq,Show)

data Bound = Bounded Int | Unbounded
    deriving (Eq,Show)

data Component
    = Component_Attribute      Attribute
    | Component_Element        Element
    | Component_SimpleType     SimpleType
    | Component_ComplexType    ComplexType
    | Component_ModelGroup     ModelGroup
    | Component_AttributeGroup AttributeGroup
    | Component_AnySimpleType
    | Component_AnyType
    | Component_AnyElem
    deriving (Eq,Show)

data NameWithType = NameWithType Name (Maybe QName)
    deriving (Eq,Show)

data FormChoice = Qualified | Unqualified
    deriving (Eq,Show)

data DerivationSet
    = AllFinal
    | NoExtension
    | NoRestriction
    deriving (Eq,Show)

data Restriction t = Restriction
    { restriction_annotation :: Maybe Annotation
    , restriction_base       :: Maybe QName
    , restriction_type       :: t
    }
    deriving (Eq,Show)

data Extension t = Extension
    { extension_annotation :: Maybe Annotation
    , extension_base       :: Maybe QName
    , extension_type       :: Maybe t
    }
    deriving (Eq,Show)

data Attribute = Attribute
    { attribute_nameOrRef       :: Either NameWithType QName
    , attribute_use             :: Use
    , attribute_default         :: Maybe String
    , attribute_fixed           :: Maybe String
    , attribute_form            :: FormChoice
    , attribute_targetNamespace :: Maybe AnyURI
    , attribute_inheritable     :: Maybe Bool
    , attribute_annotation      :: Maybe Annotation
    , attribute_simpleType      :: Maybe SimpleType
    }
    deriving (Eq,Show)

data Element = Element
    { element_nameOrRef  :: Either NameWithType QName
    , element_occurs     :: Occurs
    , element_default    :: Maybe String
    , element_fixed      :: Maybe String
    , element_nillable   :: Bool
    , element_abstract   :: Bool
    , element_substGroup :: Maybe QName
    , element_final      :: Maybe DerivationSet
    , element_block      :: Maybe DerivationSet
    , element_form       :: FormChoice
    , element_annotation :: Maybe Annotation
    , element_content    :: Maybe (Either SimpleType ComplexType)
    }
    deriving (Eq,Show)

data ComplexType = ComplexType
    { complex_annotation :: Maybe Annotation
    , complex_name       :: Maybe Name
    , complex_abstract   :: Bool
    , complex_final      :: Maybe DerivationSet
    , complex_block      :: Maybe DerivationSet
    , complex_mixed      :: Bool
    , complex_content    :: Maybe ComplexItem
    }
    deriving (Eq,Show)

-- No Attributes, no Elements, just Text
data SimpleType = SimpleType
    { simple_annotation :: Maybe Annotation
    , simple_name       :: Maybe Name
    , simple_final      :: Maybe DerivationSet
    , simple_item       :: SimpleItem
    }
    deriving (Eq,Show)

data SimpleItem
    = Restricted  SimpleRestriction
    | UnionOf     [QName] [SimpleType]
    | ListOf      (Either QName SimpleType)
    deriving (Eq,Show)

data ComplexItem
    -- restriction or extension
    = ComplexContent ComplexContent Bool {-mixed content, i.e. text-}
    -- text or a simpleType as content, may contain attributes, no elements
    | SimpleContent  SimpleContent
    -- attributes, elements
    | OtherContent   OtherContent
    deriving (Eq,Show)

type ComplexContent    = Either (Restriction OtherContent) (Extension OtherContent)
type SimpleContent     = Either SimpleRestriction (Extension Attributes)
type SimpleRestriction = Restriction (Maybe SimpleType, [Facet])

-- This type of content does not appear to have a name, but has the following grammar:
-- ((group | all | choice | sequence)?, ((attribute | attributeGroup)*, anyAttribute?))
type OtherContent = (Maybe Elements, [Attributes], Maybe AnyAttr)

data AnyAttr = AnyAttr
    { anyAttr_annotation :: Maybe Annotation
    , anyAttr_namespace  :: AnyURI
    }
    deriving (Eq,Show)

data Facet = Facet
    { facet_annotation :: Maybe Annotation
    , facet_kind       :: FacetKind
    , facet_value      :: String
    , facet_fixed      :: Bool
    }
    deriving (Eq,Show)

data FacetKind
    = Facet_Length
    | Facet_MinLength
    | Facet_MaxLength
    | Facet_MinExclusive
    | Facet_MinInclusive
    | Facet_MaxExclusive
    | Facet_MaxInclusive
    | Facet_TotalDigits
    | Facet_FractionDigits
    | Facet_Whitespace
    | Facet_Pattern
    | Facet_Enumeration
    deriving (Eq,Show)

-- | A syntactic contruct, types declared within a model
-- group become local to the type that uses the group.
data ModelGroup = ModelGroup
    { modelGroup_annotation :: Maybe Annotation
    , modelGroup_nameOrRef  :: Either Name QName
    , modelGroup_occurs     :: Occurs
    , modelGroup_content    :: Maybe Elements
    }
    deriving (Eq,Show)

-- | A syntactic contruct, types declared within an attribute
-- group become local to the type that uses the group.
data AttributeGroup = AttributeGroup
    { attributeGroup_annotation :: Maybe Annotation
    , attributeGroup_nameOrRef  :: Either Name QName
    , attributeGroup_content    :: [Attributes]
    }
    deriving (Eq,Show)

type Attributes = Either [Attribute] AttributeGroup

data Elements
    = Elements_All       (Maybe Annotation) [Element]
    | Elements_Choice    (Maybe Annotation) Occurs [Elements]
    | Elements_Sequence  (Maybe Annotation) Occurs [Elements]
    | Elements_Element   Element
    | Elements_Group     ModelGroup
    | Elements_Any
    deriving (Eq,Show)
