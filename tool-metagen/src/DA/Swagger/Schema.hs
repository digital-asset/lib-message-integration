-- Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

-- Todo: use generics to derive the {From,To}Json typeclasses

module DA.Swagger.Schema where

import Data.Aeson
--import GHC.Generics

-- Top level declaration, tranformed into DAML data type.
data Root = Root {
  rootSwagger :: String,
  rootPaths :: [Parameter]
} deriving Show

-- TODO: use genericParseJson + fieldLabelModifier to derive instances automatically
instance FromJSON Root where
  parseJSON = withObject "root" $ \o -> Root
    <$> o .: "swagger"
    <*> o .: "paths"

data Parameter = Parameter {
  parameterName :: String,
  parameterIn :: String,
  parameterDescription :: String,
  parameterRequired :: Bool,
  parameterSchema :: Schema
  } deriving Show

instance FromJSON Parameter where
  parseJSON = withObject "parameter" $ \o -> Parameter
    <$> o .: "name"
    <*> o .: "in"
    <*> o .: "description"
    <*> o .: "required"
    <*> o .: "schema"

data Schema = Schema {
  schemaRef :: Maybe String,
  schemaFormat :: String,
  schemaDescription :: String,
  schemaDefault :: Maybe Object,
  schemaMultipleOf :: Maybe Int,
  schemaMaximum :: Maybe Int,
  schemaExclusiveMaximum :: Maybe Int,
  schemaMinimum :: Maybe Int,
  schemaExclusiveMinimum :: Maybe Int,
  schemaMaxLength :: Maybe Int,
  schemaMinLength :: Maybe Int,
  schemaPattern :: Maybe String,
  schemaMaxItems :: Maybe Int,
  schemaMinItems :: Maybe Int,
  schemaUniqueItems :: Maybe Int,
  schemaMaxProperties :: Maybe Int,
  schemaMinProperties :: Maybe Int,
  schemaRequired :: [String],  
  schemaDiscriminator :: String,
  schemaReadOnly :: Bool,
  schemaType :: [String], -- Either String [String]
  schemaItems :: [Schema],
  schemaAllOf :: [Schema]
  -- _enum :: [Object]
  -- xml
  -- externalDocs
  -- example :: Data.Jaeson.Object
  } deriving Show

instance FromJSON Schema where
  parseJSON = withObject "schema" $ \o -> Schema
    <$> o .:? "$ref"
    <*> o .:  "format"
    <*> o .:  "description"
    <*> o .:? "default"
    <*> o .:? "multipleOf"
    <*> o .:? "maximum"
    <*> o .:? "exclusiveMaximum"
    <*> o .:? "minimum"
    <*> o .:? "exclusiveMinimum"
    <*> o .:? "maxLength"
    <*> o .:? "minLength"
    <*> o .:? "pattern"
    <*> o .:? "maxItems"
    <*> o .:? "minItems"
    <*> o .:? "uniqueItems"
    <*> o .:? "maxProperties"
    <*> o .:? "minProperties"
    <*> o .: "required"
    <*> o .: "discriminator"
    <*> o .: "readOnly"
    <*> o .: "type"
    <*> o .: "items"
    <*> o .: "allOf"


