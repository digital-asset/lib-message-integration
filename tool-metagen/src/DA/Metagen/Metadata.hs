-- Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Types and functions for generating JSON-based encoder/decoder metadata.
--
module DA.Metagen.Metadata where

import           DA.Daml.TypeModel

import           Data.Aeson
import           Data.List                 as List
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Text                  (Text)
import qualified Data.Text                 as T
import           Data.Version              (showVersion)

-- cabal generated
import Paths_metagen (version)

type Env a = Map Name (Decl a)

------------------------------------------------------------
-- Types for the JSON metadata file format

data Metadata a b = Metadata
    { schema         :: Text
    , format         :: Text
    , declarations   :: [TypeDecl a]
    , top_level_meta :: b
    } deriving (Eq, Ord, Show)

data TypeDecl a = TypeDecl
    { kind   :: DeclKind
    , name   :: Text
    , fields :: [FieldDecl a]
    } deriving (Eq, Ord, Show)

data FieldDecl a = FieldDecl
    { name        :: Text
    , daml_type   :: DamlType
    , cardinality :: Card
    , meta        :: a
    } deriving (Eq, Ord, Show)

data DamlType = DamlType
    { name :: Text
    , kind :: TypeKind
    } deriving (Eq, Ord, Show)

data DeclKind = RecordDecl | VariantDecl
    deriving (Eq, Ord, Show)

data TypeKind = PrimType | NamedRecordType | NamedVariantType
    deriving (Eq, Ord, Show)

data Card = MANY | MANY1 | OPTIONAL | SINGLE
    deriving (Eq, Ord, Show)

instance (ToJSON a, ToJSON b) => ToJSON (Metadata a b) where
    toJSON Metadata{..} = object
        [ "schema"           .= schema
        , "metagen_version"  .= showVersion version
        , "format"           .= format
        , "declarations"     .= declarations
        , "top_level_meta"   .= top_level_meta
        ]

instance ToJSON a => ToJSON (TypeDecl a) where
    toJSON TypeDecl{..} = object
        [ "kind"   .= kind
        , "name"   .= name
        , "fields" .= fields
        ]

instance ToJSON a => ToJSON (FieldDecl a) where
    toJSON FieldDecl{..} = object
        [ "name"        .= name
        , "type"        .= daml_type
        , "cardinality" .= cardinality
        , "meta"        .= meta
        ]

instance ToJSON DamlType where
    toJSON DamlType{..} = object
        [ "name" .= name
        , "kind" .= kind
        ]

instance ToJSON DeclKind where
    toJSON RecordDecl  = toJSON ("record" :: Text)
    toJSON VariantDecl = toJSON ("variant" :: Text)

instance ToJSON TypeKind where
    toJSON NamedRecordType  = toJSON ("record" :: Text)
    toJSON NamedVariantType = toJSON ("variant" :: Text)
    toJSON PrimType         = toJSON ("prim" :: Text)

instance ToJSON Card where
    toJSON MANY     = toJSON ("MANY" :: Text)
    toJSON MANY1    = toJSON ("MANY1" :: Text)
    toJSON OPTIONAL = toJSON ("OPTIONAL" :: Text)
    toJSON SINGLE   = toJSON ("SINGLE" :: Text)

genDecls
    :: forall a. Eq a
    => Env a
    -> [Decl a]
    -> [TypeDecl a]
genDecls env = List.nub . concatMap genDecl
  where
    genDecl :: Decl a -> [TypeDecl a]

    genDecl (EnumType name constrs _comment) =
        [TypeDecl VariantDecl (T.pack name) (map genEnumField constrs)]
      where
        genEnumField (cname, meta, _comment) =
            let (daml_type, _) = genType env (Prim PrimUnit)
            in FieldDecl (T.pack name <> "_" <> T.pack cname)
                         daml_type
                         SINGLE
                         meta

    genDecl (RecordType name fields _comment) =
        let (field_decls, type_decls) = unzip $ map (genField env) fields
        in TypeDecl RecordDecl (T.pack name) field_decls : List.concat type_decls

    genDecl (VariantType name fields _comment) =
        let (field_decls, type_decls) = unzip $ map (genField env) fields
        in TypeDecl VariantDecl (T.pack name) field_decls : List.concat type_decls
      where
        prefix f = f {field_name = name ++ "_" ++ field_name f }

    genDecl _ = []

    genField :: Env a -> Field a -> (FieldDecl a, [TypeDecl a])
    genField env Field{..} =
        let (daml_type, type_decls) = genType env field_type
            field_decl = FieldDecl
              { name        = T.pack field_name
              , daml_type   = daml_type
              , cardinality = genCard field_cardinality
              , meta        = field_meta
              }
        in (field_decl, type_decls)

genType :: (Eq a) => Env a -> Type a -> (DamlType, [TypeDecl a])
genType _ (Prim prim)                     = (genPrimType prim, [])
genType env (Nominal name)                = genNominalType env name
genType env (HigherKinded (name, types))  = genHigherKindedType env name types
genType _ Product       {}  = error "Anonymous product types not currently supported"
genType _ Sum           {} = error "Anonymous sum types not currently supported"
genType _ Enum          {} = error "Anonymous enum types not currently supported"

genPrimType :: PrimType -> DamlType
genPrimType ty = DamlType (ppPrimType ty) PrimType

ppPrimType :: PrimType -> Text
ppPrimType = \case
    PrimText    -> "TEXT"
    PrimBool    -> "BOOL"
    PrimInteger -> "INT64"
    PrimDecimal -> "DECIMAL"
    PrimTime    -> "TIME"
    PrimDate    -> "DATE"
    PrimUnit    -> "UNIT"
    PrimParty   -> "PARTY"

genNominalType :: (Eq a) => Env a -> Name -> (DamlType, [TypeDecl a])
genNominalType env name
    | isRecord name env  = (DamlType (T.pack name) NamedRecordType, [])
    | isVariant name env = (DamlType (T.pack name) NamedVariantType, [])
    | isEnum name env    = (DamlType (T.pack name) NamedVariantType, [])
    | Just base <- isNewType name env
                       = genType env base
    | otherwise        = error $ "Unsupported type: " ++ name

isRecord :: Name -> Env a -> Bool
isRecord name env =
    case Map.lookup name env of
        Just RecordType{} -> True
        _ -> False

isVariant :: Name -> Env a -> Bool
isVariant name env =
    case Map.lookup name env of
        Just VariantType{} -> True
        _ -> False

isEnum :: Name -> Env a -> Bool
isEnum name env =
    case Map.lookup name env of
        Just EnumType{} -> True
        _ -> False

isNewType :: Name -> Env a -> Maybe (Type a)
isNewType name env =
    case Map.lookup name env of
        Just (NewType _ base _) -> Just base
        _ -> Nothing

genHigherKindedType :: (Eq a) => Env a -> Name -> [Either PrimType Name] -> (DamlType, [TypeDecl a])
genHigherKindedType env name types =
    case Map.lookup name env of
      Just (HigherRecordType recordName argNames fields comment) ->
        let typeName = name ++ "_" ++ (List.intercalate "_" $ List.map ppType types)
            damlType = DamlType (T.pack typeName) NamedRecordType

            fields_new = map (applyType (Map.fromList $ zip argNames types)) fields
            typeDecls = genDecls env $ [RecordType typeName fields_new comment]
        in (damlType, typeDecls)
      _ ->  error $ "Unsupported type: " ++ name

    where
      ppType :: Either PrimType Name -> String
      ppType (Left primType) = T.unpack $ ppPrimType primType
      ppType (Right name) = name

      applyType :: Map ArgName (Either PrimType Name) -> Field a -> Field a
      applyType name2type field
        | Nominal name <- field_type field =
            case Map.lookup name name2type of
              Just (Left primType) -> field { field_type = Prim primType }
              Just (Right name) -> field { field_type = Nominal name }
              Nothing -> field
        | otherwise = field


genCard :: Cardinality -> Card
genCard (Cardinality One  ToOne)  = SINGLE
genCard (Cardinality Zero ToOne)  = OPTIONAL
genCard (Cardinality Zero ToMany) = MANY
genCard (Cardinality One  ToMany) = MANY1
