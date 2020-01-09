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
import Data.HashMap.Strict.InsOrd (toList)
import Data.Maybe (fromJust, catMaybes)
import Data.Foldable (fold)
import Data.Bifunctor
import Prelude hiding (words, lookup)
import Control.Lens
import GHC.Base ((<|>)) -- Alternative
import qualified Data.Char (toLower)

-- Used in both requests and replies
signatoryField = Field {
  field_name = "requestor",
  field_type = Prim PrimParty,
  field_cardinality = single,
  field_comment = noComment,
  field_meta = ()
}

parseSwagger :: (MonadLogger m, MonadIO m) => Swagger -> m (Module ())
parseSwagger sw =
  pure $ Module name imports (defs ++ ops) (Comment desc)
    where 
      name = sw ^. info . title . to (unpack . toCamelType)
      imports = []
      defs = catMaybes
               . snd
               . unzip
               . fmap (uncurry parseSchema)
               . toList 
               . fmap Inline 
               $ (sw ^. definitions)
      ops = toListOf allOperations sw >>= parseDecl
      desc = sw ^. info . description . to (fmap unpack) --TODO: seems the comment is not included in the output file.

parseDecl :: Operation -> [ Decl () ]
parseDecl op =
    let name = fromJust $ maybeId <|> maybeSum
        maybeSum = op ^. summary . to (fmap (unpack . toCamelType))
        maybeId = op ^. operationId . to (fmap unpack)
        comment = op ^. description . to (Comment . fmap unpack)
        (fields, maybeAnonDecls) = 
          unzip (fmap parseParam (op ^. parameters))
    in TemplateType name (signatoryField : fields) (Signatory "requestor") comment 
      : catMaybes maybeAnonDecls
      ++ (op ^. responses . to (parseResponses name))

parseResponses :: String -> Responses -> [ Decl () ]
parseResponses name r = 
  let defaultName = name ++ "ResponseDefault"
      maybeDefaultResponse = r ^. default_ . to (fmap (parseResponse defaultName))
      defaultResponse = fst <$> maybeDefaultResponse
      embeddedDefault = maybeDefaultResponse >>= snd
      codeName code = name ++ "Response" ++ show code
      (codedResponses, embeddedData) = 
        r ^. responses . to (unzip
          . fmap (uncurry parseResponse . first codeName)
          . toList
        )
  in codedResponses ++ catMaybes (defaultResponse : embeddedDefault : embeddedData)

parseResponse :: String -> Referenced Response -> (Decl (), Maybe (Decl ()))
parseResponse name (Inline (Response desc (Just refdSchema) _ _ )) = (
    TemplateType name (signatoryField: fields) (Signatory "requestor") (Comment $ Just $ unpack desc),
    Nothing
  )
    where
      fields = case snd (parseSchema "response" refdSchema) of
        Just (RecordType _ fields' _) -> fields'
        otherwise -> []

parseResponse name (Inline (Response desc Nothing _ _)) = (
    TemplateType name [signatoryField] (Signatory "requestor") (Comment $ Just $ unpack desc),
    Nothing
  )
parseResponse name (Ref _) = error "Reference response not supported yet"
 
parseParam :: Referenced Param -> (Field (), Maybe (Decl ()))
parseParam (Inline param) = case param ^. schema of
  ParamBody s -> parseSchema (param ^. name) s
  ParamOther pos -> 
    let (type', cardinality') = swaggerToDamlType "foo" (pos ^. paramSchema . type_ . to fromJust) (pos ^. paramSchema . items)
    in (
      Field 
        (param ^. name . to (unpack . toCamelVal))
        type'  
        cardinality' 
        noComment 
        (),
      Nothing
    )
parseParam (Ref (Reference _ {- path -})) = undefined
    
   
parseSchema :: Text -> Referenced Schema -> (Field (), Maybe (Decl ()))
parseSchema fieldName (Inline s) = 
  (Field {
    field_name = unpack $ toCamelVal fieldName,
    field_type = type',
    field_cardinality = cardinality', -- Ovewrwritten; see mkOptional
    field_comment = Comment $ fmap unpack (s ^. description),
    field_meta = ()
   },
   Just
     $ RecordType
         anonName
         (fmap mkOptional
            $ fst 
            $ unzip 
            $ fmap (uncurry parseSchema) (s ^. properties . to toList)
         )
         noComment
  )
  where 
    anonName = unpack $ toCamelType fieldName
    (type', cardinality') = swaggerToDamlType anonName (s ^. type_ . to fromJust) (s ^. items)
    mkOptional f = 
      if field_cardinality(f) == many ||
         elem (pack $ field_name f) (s ^. required) then f
      else Field {
        field_name = field_name (f),
        field_type = field_type (f),
        field_cardinality = optional,
        field_comment = field_comment(f),
        field_meta = field_meta(f)
      }

parseSchema fieldName (Ref (Reference path)) = (
    Field (unpack $ toCamelVal fieldName) (Nominal $ unpack path) single noComment (),
    Nothing
  )

-- SwaggerItems only required for arity > 1
swaggerToDamlType :: Name -> SwaggerType t -> Maybe (SwaggerItems t) -> (Type (), Cardinality)
swaggerToDamlType _ SwaggerString _ = (Prim PrimText, single)
swaggerToDamlType _ SwaggerNumber _ = (Prim PrimDecimal, single)
swaggerToDamlType _ SwaggerInteger _ = (Prim PrimInteger, single)
swaggerToDamlType _ SwaggerBoolean _ = (Prim PrimBool, single)
swaggerToDamlType _ SwaggerArray (Just (SwaggerItemsObject (Inline schem))) = 
  (fst (swaggerToDamlType "" (schem ^. type_. to fromJust) (schem ^. items)) , many)
swaggerToDamlType name SwaggerArray (Just (SwaggerItemsObject (Ref (Reference { getReference = ref } ) ))) = 
  (Nominal $ unpack $ toCamelType ref, many)
swaggerToDamlType _ SwaggerFile _ = error "Type 'file' not implemented yet"
swaggerToDamlType _ SwaggerNull _ = error "Type 'null' not implemented yet"
swaggerToDamlType name SwaggerObject _ = (Nominal name, single)

-- Also handles keywords.
toCamelType :: Text -> Text
toCamelType "type"= "type_" --how to bind using '@' ? 
toCamelType "data"= "data_"
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
  
