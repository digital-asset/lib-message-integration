-- Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE GADTs #-}
module DA.Swagger.Parse where

-- import qualified Data.Aeson as Aeson
-- import           Data.Aeson.Types as Aeson
import DA.Daml.TypeModel
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Swagger
import Data.Text (Text, toTitle, pack, unpack, split)
import Data.HashMap.Strict.InsOrd (elems, toList)
import Data.Maybe (fromJust, catMaybes)
import Data.Foldable (fold)
import Prelude hiding (words, lookup)
import Control.Lens
import GHC.Base ((<|>)) -- Alternative
import qualified Data.Char (toLower)

parseSwagger :: (MonadLogger m, MonadIO m) => Swagger -> m (Module ())
parseSwagger Swagger{..} =
  pure $ Module "Test" [] decls noComment
    where 
      decls = ops >>= parseDecl
      ops :: [ Operation ]
      ops = catMaybes 
        . (=<<) (\pi -> [_pathItemGet pi, _pathItemPut pi, _pathItemPost pi])
        . elems
        $ _swaggerPaths

parseDecl :: Operation -> [ Decl () ]
parseDecl op =
    let name = fromJust $ maybeId <|> maybeSum
        maybeSum = op ^. summary . to (fmap (unpack . toCamelType))
        maybeId = op ^. operationId . to (fmap unpack)
        comment = op ^. description . to (Comment . fmap unpack)
        (fields, maybeAnonDecls) = 
          unzip (fmap parseParam (op ^. parameters))
    in RecordType name fields comment : catMaybes maybeAnonDecls

parseParam :: Referenced Param -> (Field (), Maybe (Decl ()))
parseParam (Inline param) = case param ^. schema of
  ParamBody s -> parseSchema (param ^. name) s
  ParamOther pos -> (
    Field 
      (param ^. name . to (unpack . toCamelVal))
      (pos ^. paramSchema . type_ . to (swaggerToDamlType "foo" . fromJust)) 
      single 
      noComment 
      (),
    Nothing
    )
parseParam (Ref (Reference path)) = error "TODO"
    
parseSchema :: Text -> Referenced Schema -> (Field (), Maybe (Decl ()))
parseSchema fieldName (Inline s) = 
  (Field {
    field_name = unpack $ toCamelVal fieldName,
    field_type = swaggerToDamlType anonName (s ^. type_ . to fromJust),
    field_cardinality = single,
    field_comment = noComment,
    field_meta = ()
   },
   Just
     $ RecordType
         anonName
         (fst 
            $ unzip 
            $ fmap (uncurry parseSchema) (s ^. properties . to toList)
         )
         noComment
  )
  where anonName = unpack $ toCamelType fieldName

parseSchema fieldName (Ref (Reference path)) = (
    Field (unpack fieldName) (Nominal $ unpack path) single noComment (),
    Nothing
  )

swaggerToDamlType :: Name -> SwaggerType t -> Type ()
swaggerToDamlType _ SwaggerString = Prim PrimText
swaggerToDamlType _ SwaggerNumber = Prim PrimDecimal
swaggerToDamlType _ SwaggerInteger = Prim PrimInteger
swaggerToDamlType _ SwaggerBoolean = Prim PrimBool
swaggerToDamlType _ SwaggerArray = error "Type 'array' not implemented yet"
swaggerToDamlType _ SwaggerFile = error "Type 'file' not implemented yet"
swaggerToDamlType _ SwaggerNull = error "Type 'null' not implemented yet"
swaggerToDamlType name SwaggerObject = Nominal name

-- Also handles keywords.
toCamelType :: Text -> Text
toCamelType "type" = "type_"
toCamelType s = fold $ fmap toTitle $ split (\a -> case a of
    ' ' -> True
    '_' -> True
    '-' -> True
    _ -> False
  ) s

toCamelVal :: Text -> Text
toCamelVal t = case unpack (toCamelType t) of 
  c : cs -> pack $ Data.Char.toLower c : cs
  empty -> pack empty
  
