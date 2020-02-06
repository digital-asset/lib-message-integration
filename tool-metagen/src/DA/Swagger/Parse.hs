-- Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE GADTs #-}
module DA.Swagger.Parse where

-- import qualified Data.Aeson as Aeson
-- import           Data.Aeson.Types as Aeson
import DA.Daml.TypeModel
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.State (State, runState, execState, modify)
import Data.Swagger
import Data.Text (Text, pack, unpack, split)
import Data.HashMap.Strict.InsOrd (InsOrdHashMap, insert, traverseWithKey, empty, elems)
import Data.Maybe (fromMaybe)
import Data.Foldable (fold)
import Prelude hiding (words, lookup)
import Control.Lens
import GHC.Base ((<|>)) -- Alternative
import qualified Data.Char (toLower, toUpper)

type Field_ = Field ()
-- Functions returning a SymbolState a will return the top-level declaration(s)
-- a, and embedded declarations in the monad (e.g. when a field is of type 
-- `object`, a corresponding `data` declaration is required at the top level.
type SymbolState = State (InsOrdHashMap Text (Decl()))

-- Used in both requests and replies
signatoryField :: Field_
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
      defs = parseDefns sw
      ops = parseOps sw
      desc = sw ^. info . description . to (fmap unpack) --TODO: seems the comment is not included in the output file.

parseOps :: Swagger -> [ Decl () ]
parseOps s = case runState (traverse parseOp (toListOf allOperations s)) empty of
    (ops, decls) -> concat ops ++ elems decls

parseOp :: Operation -> SymbolState [ Decl () ]
parseOp op =
  do let name = fromMaybe (error $"missing `operationId` and `summary` in " <> show op) (maybeId <|> maybeSum)
         maybeSum = op ^. summary . to (fmap unpack)
         maybeId = op ^. operationId . to (fmap unpack)
         comment = op ^. description . to (Comment . fmap unpack)
     params <- traverse parseParam (op ^. parameters)
     reps <- parseResponses name (op ^. responses)
     return $ TemplateType (unpack . toCamelType . pack $ name) (signatoryField : params) (Signatory "requestor") comment : reps

parseResponses :: String -> Responses -> SymbolState [ Decl () ]
parseResponses name r = 
  do 
    repMap <- traverseWithKey parseBody (r ^. responses)
    return $ 
      [ TemplateType 
          typeName
          [signatoryField, body]
          (Signatory "requestor")
          noComment
        ,
        VariantType -- see TypeModel.hs for how this is converted to variant
          (typeName <> "Body")
          (elems repMap)
          noComment
      ]
  where 
    typeName = unpack ((toCamelType $ pack name) <> "Response")
    body = Field {
      field_name = "body",
      field_type = Nominal $ typeName <> "Body",
      field_cardinality = single,
      field_comment = noComment,
      field_meta = ()
    }
    parseBody :: (Show a) => a -> Referenced Response -> SymbolState Field_
    parseBody code (Inline (Response _desc (Just refdSchema) _ _ )) = 
      parseSchema (pack ( name <> " response body " <> show code)) refdSchema
    parseBody code (Inline (Response _desc Nothing _ _ )) = 
      pure $ Field {
                field_name = unpack $ toCamelVal $ pack $ name <> " response body " <> show code,
                field_type = Product [], -- use instead of Unit as type not supported in DAML.
                field_cardinality = single,
                field_comment = noComment,
                field_meta = ()
             }
 
parseParam :: Referenced Param -> SymbolState Field_
parseParam (Inline param) = case param ^. schema of
  ParamBody s -> parseSchema (param ^. name) s
  ParamOther pos -> 
    let (type', cardinality') = 
           swaggerToDamlType 
             ""
             (pos ^. paramSchema . type_ . to (fromMaybe SwaggerString)) -- Return untyped string value by default.
             (pos ^. paramSchema . items)
    in pure $
      Field 
        (param ^. name . to (unpack . toCamelVal))
        type'  
        cardinality' 
        noComment 
        ()
parseParam (Ref (Reference _path)) = undefined
    
parseDefns :: Swagger -> [ Decl () ]
parseDefns s = elems (execState (traverseWithKey parseSchema (fmap Inline (s ^. definitions))) empty) 

parseSchema :: Text -> Referenced Schema -> SymbolState Field_
parseSchema fieldName (Inline s) = 
  do 
    let typeName = unpack $ toCamelType fieldName
        valName = unpack $ toCamelVal fieldName
        (type', cardinality') = swaggerToDamlType 
           typeName 
           (s ^. type_ . to (fromMaybe SwaggerString)) -- Return untyped string value by default.
           (s ^. items)
    fields <- traverseWithKey parseSchema (s ^. properties)
    modify $ case type' of
      Prim _ -> id
      _ -> insert (pack typeName) (RecordType typeName (elems $ fmap mkOptional fields) noComment) --FIXME: handle namespace clashes
    return $ Field {
      field_name = valName,
      field_type = type',
      field_cardinality = cardinality', -- Ovewrwritten; see mkOptional
      field_comment = Comment $ fmap unpack (s ^. description),
      field_meta = ()
     }
  where 
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

parseSchema fieldName (Ref (Reference path)) = 
    return $ Field {
               field_name = unpack . toCamelVal $ fieldName,
               field_type = Nominal . unpack . toCamelType $ path,
               field_cardinality = single,
               field_comment = noComment,
               field_meta = ()
            }

-- SwaggerItems only required for arity > 1
swaggerToDamlType :: Name -> SwaggerType t -> Maybe (SwaggerItems t) -> (Type (), Cardinality)
swaggerToDamlType _ SwaggerString _ = (Prim PrimText, single)
swaggerToDamlType _ SwaggerNumber _ = (Prim PrimDecimal, single)
swaggerToDamlType _ SwaggerInteger _ = (Prim PrimInteger, single)
swaggerToDamlType _ SwaggerBoolean _ = (Prim PrimBool, single)
swaggerToDamlType _ SwaggerArray (Just (SwaggerItemsObject (Inline schem))) = 
  (fst (swaggerToDamlType "" (schem ^?! type_ . folded) (schem ^. items)) , many)
swaggerToDamlType _ SwaggerArray (Just (SwaggerItemsObject (Ref (Reference { getReference = ref } ) ))) = 
  (Nominal $ unpack $ toCamelType ref, many)
swaggerToDamlType _ SwaggerArray (Just (SwaggerItemsPrimitive _ _)) = error "Type 'array' (primitive) not implemented"
swaggerToDamlType _ SwaggerArray (Just (SwaggerItemsArray _)) = error "Type 'array' (heterogenous) not supported"
swaggerToDamlType _ SwaggerArray Nothing = (Prim PrimText, many) -- Use this as it apperas in query params mainly. Consider (Nominal "a", many). 
swaggerToDamlType _ SwaggerFile _ = error "Type 'file' not implemented yet"
swaggerToDamlType _ SwaggerNull _ = error "Type 'null' not implemented yet"
swaggerToDamlType name SwaggerObject _ = (Nominal name, single)

-- Also handles keywords.
toCamelType :: Text -> Text
toCamelType "type"= "Type_" --how to bind using '@' ? 
toCamelType "data"= "Data_"
toCamelType s = fold $ fmap toTitle $ split (\a -> case a of --FIXME: toTitle differs from scala capitalize (trailing characters are not touched).
    ' ' -> True
    '_' -> True
    '-' -> True
    _ -> False
  ) s

toCamelVal :: Text -> Text
toCamelVal t = case unpack (toCamelType t) of 
  c : cs -> pack $ Data.Char.toLower c : cs
  empty -> pack empty
  
-- This is different from Data.Text in that it only operates on the first char.
-- e.g. `Data.Text.toTitle "API" -> "Api"` whereas here we get "API back unchanged.
toTitle :: Text -> Text
toTitle s = pack $ case unpack s of
     h : t -> Data.Char.toUpper h : t
     [] -> []
