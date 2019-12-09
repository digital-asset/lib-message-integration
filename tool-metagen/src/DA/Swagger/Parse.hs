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
import Control.Lens.Getter

type Conv = Reader Swagger

parseSwagger :: (MonadLogger m, MonadIO m) => Swagger -> m (Module ())
parseSwagger env@Swagger{..} =
  pure $ Module "Test" [] (runReader (sequence decls) env) noComment
    where 
      decls :: [ Conv (Decl ()) ]
      decls = map parseDecl 
        $ catMaybes 
        . map _pathItemGet 
        . elems
        $ _swaggerPaths

parseDecl :: Operation -> Conv (Decl ())
parseDecl op = do 
    name <- op ^. summary . to (pure . unpack . deriveName . fromJust)
    let refdFields = ((op ^. parameters) :: [Referenced Param])
    fields <- traverse parseField refdFields
    let comment = op ^. description . to (\s -> Comment $ fmap unpack s)
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
  schema <- asks (\ swag -> fromJust ( lookup path (_swaggerDefinitions swag)))
  return schema

derefParam :: Referenced Param -> Conv Param
derefParam (Inline a) = pure a
derefParam (Ref (Reference path)) = do
  param <- asks (\ swag -> fromJust ( lookup path (_swaggerParameters swag)))
  return param

swaggerToDamlType :: SwaggerType t -> Type ()
swaggerToDamlType SwaggerString = Prim PrimText

-- "Hello world!" -> "HelloWorld!"
deriveName :: Text -> Text
deriveName s = fold $ fmap toTitle $ words s
