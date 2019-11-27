-- Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

-- Todo: use generics to derive the {From,To}Json typeclasses

module DA.Swagger.Schema where

import Data.Aeson
-- import Data.Aeson.Types (Parser)
import Data.HashMap.Strict (HashMap) --, toList)
import Data.Text (Text)
--import GHC.Generics
--import Data.List.NonEmpty (NonEmpty)

-- Top level declaration, tranformed into DAML data type.
data Root = Root {
  rootSwagger :: Text,
  rootPaths :: HashMap Text PathItemObject
} deriving Show

-- TODO: use genericParseJson + fieldLabelModifier to derive instances automatically
instance FromJSON Root where
  parseJSON = withObject "root" $ \o -> Root
    <$> o .: "swagger"
    <*> o .: "paths"

data PathItemObject = PathItemObject {  -- consider using a coproduct
  -- pathItemObjectRef :: Maybe Text,
  pathItemObjectGet :: Maybe OperationObject,
  pathItemObjectPut :: Maybe OperationObject,
  pathItemObjectPost :: Maybe OperationObject
  -- pathItemObjectParameters :: Maybe [ParameterObject] --global params
  }
  deriving Show

instance FromJSON PathItemObject where
  parseJSON = withObject "pathItem" $ \o -> PathItemObject 
    <$> o .:? "get"
    <*> o .:? "put"
    <*> o .:? "post"

data OperationObject = OperationObject {
  parameters :: Maybe [ParameterObject] -- Either ReferenceObject
} deriving Show

instance FromJSON OperationObject where
  parseJSON = withObject "operation" $ \o -> 
    OperationObject <$> o .:? "parameters"

data ParameterObject = ParameterObject {
  parameterObjectName :: Text,
  parameterObjectIn :: Text,
  parameterObjectDescription :: Maybe Text,
  parameterObjectRequired :: Bool,
  parameterObjectSchema :: Maybe Schema,
  parameterObjectType :: Maybe Text,
  parameterObejctFormat :: Maybe Text
  -- There's a whole bunch of other fields here
  } deriving Show

instance FromJSON ParameterObject where
  parseJSON = withObject "parameter" $ \o -> ParameterObject
    <$> o .: "name"
    <*> o .: "in"
    <*> o .:? "description"
    <*> o .: "required"
    <*> o .:? "schema"
    <*> o .:? "type"
    <*> o .:? "format"

data Schema = Schema {
  schemaRef :: Maybe Text,
  schemaFormat :: Maybe Text,
  schemaDescription :: Maybe Text,
  schemaDefault :: Maybe Object,
  schemaType :: Text, -- can be NEL of Text too
  schemaItems :: Maybe (HashMap Text Schema), -- FIXME, may be a single object
  schemaProperties :: Maybe (HashMap Text Schema) -- FIXME may be a single object
  } deriving Show

instance FromJSON Schema where
  parseJSON = withObject "schema" $ \o -> Schema
    <$> o .:? "$ref"
    <*> o .:? "format"
    <*> o .:? "description"
    <*> o .:? "default"
    <*> o .: "type"
    <*> o .:? "items"
    <*> o .:? "properties"
   -- <*> o .:? "properties"

-- Parses either a single schema, or multiple schemas in an object.
-- .:?? :: Value -> Text -> NonEmpty
