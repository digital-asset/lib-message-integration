-- Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

module DA.Swagger.Parse where

-- import qualified Data.Aeson as Aeson
-- import           Data.Aeson.Types as Aeson
import qualified DA.Swagger.Schema as Swagger
import DA.Daml.TypeModel
import           Control.Monad.Logger
import           Control.Monad.Reader

convert :: (MonadLogger m, MonadIO m) => Swagger.Root -> m (Module ())
convert Swagger.Root{..} = 
  do pure $ Module "Test" [] [] noComment
