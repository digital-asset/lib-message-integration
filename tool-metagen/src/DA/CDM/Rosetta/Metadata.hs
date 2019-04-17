-- Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

module DA.CDM.Rosetta.Metadata where

import           Data.Aeson
import           DA.Daml.TypeModel
import           DA.CDM.Rosetta.Convert    (CdmMeta (..))
import qualified Data.Map                  as Map
import           Data.Maybe
import           Data.Text                 (Text)
import qualified Data.Text                 as T

import           DA.Metagen.Metadata

-- | Convert a TypeModel to encoder/decoder metadata for CDM support.
genMetadata
    :: Module CdmMeta
    -> Metadata CdmMeta ()
genMetadata Module{..} =
    Metadata (T.pack module_name) "CDM" (genDecls env module_decls) ()
  where
    env = Map.fromList [ (name, decl)
                       | decl <- module_decls
                       , name <- maybeToList (getTypeName decl)
                       ]

instance ToJSON CdmMeta where
    toJSON (CdmField name ty) = object
        [ "src"   .= ("field"::Text)
        , "name"  .= name
        , "type"  .= ty
        ]
    toJSON (CdmEnum name) = object
        [ "src"   .= ("enum"::Text)
        , "name"  .= name
        ]
