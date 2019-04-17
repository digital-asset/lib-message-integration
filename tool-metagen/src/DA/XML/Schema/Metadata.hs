-- Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

module DA.XML.Schema.Metadata where

import           Data.Aeson
import           DA.Daml.TypeModel
import           DA.XML.Schema.Convert     (XmlMeta (..), XmlTopLevelMeta (..))
import           Text.XML.Light.Types      (QName (..))

import qualified Data.Map                  as Map
import qualified Data.Set                  as Set

import           Data.Text                  (Text)
import qualified Data.Text                 as T

import           Data.Maybe

import           DA.Metagen.Metadata


-- | Convert a TypeModel to encoder/decoder metadata for XML and XSD support.
genMetadata
    :: XmlTopLevelMeta QName (Type XmlMeta)
    -> Module XmlMeta
    -> Metadata XmlMeta (XmlTopLevelMeta Name DamlType)
genMetadata topMeta Module{..} =
    Metadata (T.pack module_name) "XSD" (genDecls env module_decls)
             (topMeta
                  { xmlTopElemTypes  = Map.mapKeys qName . fmap genType' $ xmlTopElemTypes topMeta
                  , xmlTopElemSubs   = Map.mapKeys qName . fmap (map qName) $ xmlTopElemSubs topMeta
                  , xmlBaseTypes     = Set.map genType' $ xmlBaseTypes topMeta
                  , xmlAbstractTypes = Set.map genType' $ xmlAbstractTypes topMeta
                  })
  where
    env = Map.fromList [ (name, decl)
                       | decl <- module_decls
                       , name <- maybeToList (getTypeName decl)
                       ]

    genType' = genType env

instance ToJSON XmlMeta where
    toJSON (XmlElement name) = object [ "src" .= ("element"::Text), "name" .= qName name ]
    toJSON (XmlElementRef name) = object [ "src" .= ("element-ref"::Text), "name" .= qName name]
    toJSON (XmlAttribute name) = object [ "src" .= ("attribute"::Text), "name" .= qName name]
    toJSON (XmlEnum name) = object [ "src" .= ("enum"::Text), "name" .= qName name]
    toJSON XmlChoice = object [ "src" .= ("choice"::Text) ]
    toJSON XmlSequence = object [ "src" .= ("sequence"::Text) ]
    toJSON XmlContent = object [ "src" .= ("content"::Text) ]
    toJSON XmlAny = object [ "src" .= ("any"::Text) ]

instance (ToJSONKey n, ToJSON n, ToJSON t) => ToJSON (XmlTopLevelMeta n t) where
    toJSON XmlTopLevelMeta{..} = object
        [ "namespace" .= xmlNamespace
        , "schema_version" .= xmlSchemaVersion
        , "schema_location" .= xmlSchemaLocation
        , "top_level_elements" .= xmlTopElemTypes
        , "top_level_substs" .= xmlTopElemSubs
        , "base_types" .= xmlBaseTypes
        , "abstract_types" .= xmlAbstractTypes
        ]
