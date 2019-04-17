-- Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

-- | This module is an attempt to model the undocumented and very
-- adhoc "Rosetta" language.

module DA.CDM.Rosetta.Schema where

import Prelude hiding (Enum)
import Data.Set (Set)

newtype Namespace  = Namespace  { unNamespace  :: String } deriving (Eq, Ord, Show)
newtype Identifier = Identifier { unIdentifier :: String } deriving (Eq, Ord, Show)
newtype Annotation = Annotation { unAnnotation :: String } deriving (Eq, Ord, Show)
newtype Style      = Style      { unStyle  :: Identifier } deriving (Eq, Ord, Show)

data Schema = Schema
    { schemaNamespace :: Namespace
    , schemaDecls     :: [Decl]
    } deriving Show

data Decl
    = ClassDecl       Class
    | EnumDecl        Enum
    | DataRuleDecl    DataRule
    | ChoiceRuleDecl  ChoiceRule
    | IsEventDecl     IsEvent
    | IsProductDecl   IsProduct
    | AliasDecl       Alias
    deriving Show

data Enum = Enum
    { enumName       :: Identifier
    , enumBase       :: Maybe Identifier
    , enumAnnotation :: Maybe Annotation
    , enumFields     :: [EnumField]
    } deriving Show

data Class = Class
    { classAbstract   :: Bool
    , className       :: Identifier
    , classBase       :: Maybe Identifier
    , classMeta       :: Set ClassMeta
    , classAnnotation :: Maybe Annotation
    , classFields     :: [ClassField]
    } deriving Show

-- TODO use choice rules to generate sum types?
data ChoiceRule = ChoiceRule
    { choiceRuleName       :: Identifier
    , choiceRuleAnnotation :: Maybe Annotation
    } deriving Show

-- TODO
data DataRule = DataRule
    { dataRuleName       :: Identifier
    , dataRuleAnnotation :: Maybe Annotation
    } deriving Show

-- TODO
data IsEvent = IsEvent
    { isEventIdent      :: Identifier
    , isEventAnnotation :: Maybe Annotation
    } deriving Show

-- TODO
data IsProduct = IsProduct
    { isProductIdent      :: Identifier
    , isProductAnnotation :: Maybe Annotation
    } deriving Show

-- TODO
data Alias = Alias
    { aliasIdent      :: Identifier
    , aliasAnnotation :: Maybe Annotation
    } deriving Show

data ClassField = ClassField
    { classFieldName       :: Identifier
    , classFieldType       :: Maybe Identifier -- sometimes they have just "id (0..1);"
    , classFieldCard       :: Cardinality
    , classFieldId         :: Bool
    , classFieldMeta1      :: Set FieldMeta1
    , classFieldMeta2      :: Set FieldMeta2
    , classFieldAnnotation :: Maybe Annotation
    } deriving Show

data EnumField = EnumField
    { enumFieldName        :: Identifier
    , enumFieldDisplayName :: Maybe String
    , enumFieldStyle       :: Maybe Style
    , enumFieldAnnotation  :: Maybe Annotation
    } deriving Show

data ClassMeta = COneOf | CRosettaKey | CRosettaKeyValue
    deriving (Eq, Ord, Show)

data FieldMeta1 = FAnchor | FScheme | FId
    deriving (Eq, Ord, Show)

data FieldMeta2 = FRosettaKey | FRosettaKeyValue | FReference
    deriving (Eq, Ord, Show)

data Cardinality = Cardinality
    { cardLower :: Integer
    , cardUpper :: Bound
    } deriving (Eq, Show)

data Bound = Bounded Integer | Unbounded
    deriving (Eq, Show)
