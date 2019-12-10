-- Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE TypeApplications #-}
module DA.Swagger.Parse where

-- import qualified Data.Aeson as Aeson
-- import           Data.Aeson.Types as Aeson
import DA.Daml.TypeModel
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Swagger
import Data.Text (Text, toTitle, words, unpack)
import Data.HashMap.Strict.InsOrd (elems, lookup)
import Data.Maybe (fromJust, catMaybes)
import Data.Foldable (fold)
import Prelude hiding (words, lookup)
import Control.Lens
import GHC.Base ((<|>)) -- Alternative

type Conv = Reader Swagger

parseSwagger :: (MonadLogger m, MonadIO m) => Swagger -> m (Module ())
parseSwagger env@Swagger{..} =
  pure $ Module "Test" [] (runReader (sequence decls) env) noComment
    where 
      decls :: [ Conv (Decl ()) ]
      decls = map parseDecl 
        $ catMaybes 
        . map _pathItemGet  --TODO: put, post
        . elems
        $ _swaggerPaths

parseDecl :: Operation -> Conv (Decl ())
parseDecl op = do 
    fields <- traverse parseField (op ^. parameters)
    let maybeSum = op ^. summary . to (fmap (unpack . deriveName))
    let maybeId = op ^. operationId . to (fmap unpack)
    let name = fromJust $ maybeId <|> maybeSum
    let comment = op ^. description . to (Comment . fmap unpack)
    return $ RecordType name fields comment

parseField :: Referenced Param -> Conv (Field ())
parseField param = do
    param <- derefParam param
    let name' = param ^. name . to unpack
    tpe <- param ^. schema . to parseParamSchema
    return $ Field name' tpe single noComment ()

parseParamSchema :: ParamAnySchema -> Conv (Type ())
parseParamSchema (ParamBody refdSchema) = do
  schema <- derefSchema refdSchema
  return $ swaggerToDamlType (schema ^. type_ . to fromJust)
parseParamSchema (ParamOther pos) =
  pos ^. paramSchema . type_ . to (pure . swaggerToDamlType . fromJust)

derefSchema :: Referenced Schema -> Conv Schema
derefSchema (Inline a) = pure a
derefSchema (Ref (Reference path)) = do
  schema <- asks $ fromJust . lookup path . _swaggerDefinitions
  return schema

derefParam :: Referenced Param -> Conv Param
derefParam (Inline a) = pure a
derefParam (Ref (Reference path)) = do
  param <- asks $ fromJust . lookup path . _swaggerParameters
  return param

swaggerToDamlType :: SwaggerType t -> Type ()
swaggerToDamlType SwaggerString = Prim PrimText
swaggerToDamlType SwaggerNumber = Prim PrimDecimal
swaggerToDamlType SwaggerInteger = Prim PrimInteger
swaggerToDamlType SwaggerBoolean = Prim PrimBool
-- swaggerToDamlType SwaggerArray = 
-- swaggerToDamlType SwaggerFile = 
-- swaggerToDamlType SwaggerNull = 
-- swaggerToDamlType SwaggerObject = 

-- "Hello world!" -> "HelloWorld!"
deriveName :: Text -> Text
deriveName s = fold $ fmap toTitle $ words s
