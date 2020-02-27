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
import Data.HashMap.Strict.InsOrd (InsOrdHashMap, insert, traverseWithKey, empty, elems, mapKeys, fromList, toList)
import Data.Hashable
import Data.Maybe (fromMaybe)
import Data.Foldable (fold)
import Data.Bifunctor
import Prelude hiding (words, lookup)
import System.FilePath
import Control.Lens
import GHC.Base ((<|>)) -- Alternative
import qualified Data.Char (toLower, toUpper)

type Field_ = Field ()
-- Functions returning a SymbolState a will return the top-level declaration(s)
-- a, and embedded declarations in the monad (e.g. when a field is of type 
-- `object`, a corresponding `data` declaration is required at the top level.
type SymbolState = State (InsOrdHashMap String (Decl()))

-- Used in both requests and replies
signatoryField :: Field_
signatoryField = Field {
  field_name = "requestor",
  field_type = Prim PrimParty,
  field_cardinality = single,
  field_comment = noComment,
  field_meta = ()
}

requestIdField :: Name -> Field_
requestIdField name = Field {
  field_name = "requestId",
  field_type = Nominal $ "ContractId " <> name,
  field_cardinality = single,
  field_comment = noComment,
  field_meta = ()
}

parseSwagger :: (MonadLogger m, MonadIO m) => Swagger -> m (Module ())
parseSwagger sw =
  pure $ Module name imports (defs ++ ops) (Comment desc)
    where 
      name = sw ^. info . title . to (toCamelType . unpack)
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
     params <- traverse (parseParam name) (op ^. parameters)
     reps <- parseResponses name (op ^. responses)
     return $ TemplateType (toCamelType name) (signatoryField : params) (Signatory "requestor") comment : reps

parseResponses :: String -> Responses -> SymbolState [ Decl () ]
parseResponses name r = 
  do 
    repMap <- traverseWithKey parseBody (r ^. responses . to (mapKeys show) )
    defaultRep <- traverseWithKey parseBody $ fromList (r ^.. default_ . traverse . to ("default",))
    return $ 
      [ TemplateType 
          typeName
          [signatoryField, requestIdField name, body]
          (Signatory "requestor")
          noComment
        ,
        VariantType -- see TypeModel.hs for how this is converted to variant
          (typeName <> "Body")
          (elems $ repMap <> defaultRep)
          noComment
      ]
  where 
    typeName = toCamelType name <> "Response"
    body = Field {
      field_name = "body",
      field_type = Nominal $ typeName <> "Body",
      field_cardinality = single,
      field_comment = noComment,
      field_meta = ()
    }
    parseBody :: FilePath -> Referenced Response -> SymbolState Field_
    parseBody code (Inline (Response _desc (Just refdSchema) _ _ )) = 
      parseSchema (name <> " response body " </> code) refdSchema
    parseBody code (Inline (Response _desc Nothing _ _ )) = 
      pure $ Field {
                field_name = code,
                field_type = Product [], -- use instead of Unit as type not supported in DAML.
                field_cardinality = single,
                field_comment = noComment,
                field_meta = ()
             }
    parseBody _ (Ref _) = undefined
 
parseParam :: String -> Referenced Param -> SymbolState Field_
parseParam opName (Inline param) = case param ^. schema of
  ParamBody s -> parseSchema (param ^. name . to (\x -> opName </> unpack x)) s
  ParamOther pos ->
    let (type', cardinality') = 
           swaggerToDamlType 
             (param ^. name . to (toCamelType . unpack))
             (pos ^. paramSchema . type_ . to (fromMaybe SwaggerString)) -- Return untyped string value by default.
             (pos ^. paramSchema . items)
    in pure $
      Field 
        (param ^. name . to (toCamelVal . unpack))
        type'  
        cardinality' 
        (Comment $ param ^. description . to (fmap unpack))
        ()
parseParam _opName (Ref (Reference _path)) = undefined
    
parseDefns :: Swagger -> [ Decl () ]
parseDefns s = elems (execState (traverseWithKey parseSchema (DA.Swagger.Parse.bimap unpack Inline (s ^. definitions))) empty) 

parseSchema :: FilePath -> Referenced Schema -> SymbolState Field_
parseSchema fieldPath (Inline s) = 
  do 
    let typeName = toCamelType fieldPath
        valName = toCamelVal $ takeBaseName fieldPath 
        (type', cardinality') = swaggerToDamlType 
           typeName 
           (s ^. type_ . to (fromMaybe SwaggerString)) -- Return untyped string value by default.
           (s ^. items)
    fields <- traverseWithKey parseSchema (s ^. properties . to (mapKeys (\x -> fieldPath </> unpack x)))
    modify $ case type' of
      Prim _ -> id
      _ -> insert typeName (RecordType typeName (elems $ fmap mkOptional fields) noComment)
    return $ Field {
      field_name = valName,
      field_type = type',
      field_cardinality = cardinality', -- Ovewrwritten; see mkOptional
      field_comment = Comment $ fmap unpack (s ^. description),
      field_meta = ()
     }
  where 
    -- copies a field, overriding the `cardinality` member if parent marked it optional.
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

parseSchema fieldPath (Ref (Reference path)) = 
    return $ Field {
               field_name = toCamelVal $ takeBaseName fieldPath,
               field_type = Nominal . toCamelType . unpack $ path,
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
swaggerToDamlType name SwaggerArray (Just (SwaggerItemsObject (Inline schem))) = 
  (fst (swaggerToDamlType name (schem ^. type_ . to (fromMaybe SwaggerString)) (schem ^. items)) , many)
swaggerToDamlType _ SwaggerArray (Just (SwaggerItemsObject (Ref (Reference { getReference = ref } ) ))) = 
  (Nominal . toCamelType . unpack $ ref, many)
swaggerToDamlType _ SwaggerArray (Just (SwaggerItemsPrimitive _ _)) = error "Type 'array' (primitive) not implemented"
swaggerToDamlType _ SwaggerArray (Just (SwaggerItemsArray _)) = error "Type 'array' (heterogenous) not supported"
swaggerToDamlType _ SwaggerArray Nothing = (Prim PrimText, many) -- Use this as it apperas in query params mainly. Consider (Nominal "a", many). 
swaggerToDamlType _ SwaggerFile _ = error "Type 'file' not implemented yet"
swaggerToDamlType _ SwaggerNull _ = error "Type 'null' not implemented yet"
swaggerToDamlType name SwaggerObject _ = (Nominal name, single)

-- Also handles keywords.
toCamelType :: String -> String
toCamelType "type"= "Type_" --how to bind using '@' ? 
toCamelType "data"= "Data_"
toCamelType s = fold $ fmap (toTitle . unpack) $ split (\a -> case a of
    ' ' -> True
    '_' -> True
    '-' -> True
    '.' -> True
    '/' -> True
    _ -> False
  ) $ pack s

toCamelVal :: String -> String
toCamelVal t = case toCamelType t of 
  c : cs -> Data.Char.toLower c : cs
  empty -> empty
  
-- This is different from Data.Text in that it only operates on the first char.
-- e.g. `Data.Text.toTitle "API" -> "Api"` whereas here we get "API back unchanged.
toTitle :: String -> String
toTitle (h : t) = Data.Char.toUpper h : t
toTitle [] = []

bimap :: (Eq k, Eq k', Hashable k, Hashable k') => (k -> k') -> (v -> v') -> InsOrdHashMap k v -> InsOrdHashMap k' v'
bimap f g m = fromList (fmap (Data.Bifunctor.bimap f g) (toList m))
