-- Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

-- | This represents a generic DAML type model which we can
-- easily map to DAML and DAML-LF.

module DA.Daml.TypeModel where

import Control.Arrow ((&&&))
import Control.Monad.State (State, evalState, modify, gets)
import Control.Monad.Writer (WriterT(..), tell{-, censor-})
import Control.Monad.Trans
import Data.Semigroup (Semigroup, (<>))
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

type Name = String

newtype Comment = Comment { unComment :: Maybe String }
    deriving (Eq, Ord, Show)

noComment :: Comment
noComment = Comment Nothing

-- | Top-level module with optional field annotations.
data Module a = Module
    { module_name      :: Name
    , module_imports   :: [Import]
    , module_decls     :: [Decl a]
    , module_comment   :: Comment
    }
    deriving (Eq, Show)

data Import
    = Unqualified Name
    | Qualified Name Name
    deriving (Eq, Show)

-- | Top-level declaration with optional field annotations.
data Decl a
    = EnumType      Name [(Name, a, Comment)] Comment
    | RecordType    Name [Field a] Comment
    | VariantType   Name [Field a] Comment
    | NewType       Name (Type a) Comment
    | TemplateType   Name [Field a] Signatory Comment
    | InlineComment String
    deriving (Eq, Show)

newtype Signatory = Signatory { toString :: String }
  deriving (Eq, Show)

getTypeName :: Decl a -> Maybe Name
getTypeName (EnumType n _ _)    = Just n
getTypeName (RecordType n _ _)  = Just n
getTypeName (VariantType n _ _) = Just n
getTypeName (NewType n _ _)     = Just n
getTypeName (TemplateType n _ _ _) = Just n
getTypeName (InlineComment _) = Nothing

data Field a = Field
    { field_name        :: Name
    , field_type        :: Type a
    , field_cardinality :: Cardinality
    , field_comment     :: Comment
    , field_meta        :: a
    }
    deriving (Eq, Ord, Show)

-- | Primitive or structural type (anonymous)
-- NOTE: Structural types are currently eliminated, but this decision may
-- be revisted in some cases, e.g. simple pairs.
data Type a
    = Prim    PrimType             -- ^ primitive DAML-LF types
    | Nominal Name                 -- ^ a reference to a record or variant declaration
    | Product [Field a]            -- ^ structural product (currently not serialisable in DAML-LF)
    | Sum     [Field a]            -- ^ structural sum (currently not serialisable in DAML-LF)
    | Enum    [(Name, a, Comment)] -- ^ structural enum (a special case of sum)
    -- the following better supports models with inheritance such as
    -- XSD and CDM Rosetta.
    -- Extend  [Field a] (Type a)
    deriving (Eq, Ord, Show)

data PrimType
    = PrimText
    | PrimBool
    | PrimInteger
    | PrimDecimal
    | PrimTime
    | PrimDate
    | PrimUnit
    | PrimParty
    deriving (Eq, Ord, Show)

data Lower = Zero | One
    deriving (Eq, Ord, Show)

data Upper = ToOne | ToMany
    deriving (Eq, Ord, Show)

data Cardinality = Cardinality Lower Upper
    deriving (Eq, Ord, Show)

single :: Cardinality
single = Cardinality One ToOne

optional :: Cardinality
optional = Cardinality Zero ToOne

many :: Cardinality
many = Cardinality Zero ToMany

many1 :: Cardinality
many1 = Cardinality One ToMany

instance Semigroup Cardinality where
    (<>) (Cardinality l u) (Cardinality l' u') =
        Cardinality (min l l') (max u u')

--------------------------------------------------------------

applyCard :: Cardinality -> Field a -> Field a
applyCard c f =
    f { field_cardinality =  c <> field_cardinality f }

-- | Lift all local anonymous types to the top-level, name them using
-- the enclosing type; and replace their definition with the a
-- reference to this name.  This is arguably easier for our users to
-- deal with than types such as tuples, "Either" or "OneOf3". Such
-- anonymous types are also not (yet) supported by the integration
-- adapter metadata.
--
-- NOTE: Using the above naming scheme, there are some clashes which
-- we resolve by appending a number.
typeLift :: forall a. [Decl a] -> [Decl a]
typeLift inDecls =
    let (decls, decls') = evalState (runWriterT $ mapM doDecl inDecls) mempty
    in decls ++ decls'
  where
    doDecl :: Decl a -> WriterT [Decl a] (State (Map Name Int)) (Decl a)
    doDecl (RecordType ty fields c) = do
        fields' <- mapM (doField ty) fields
        return $ RecordType ty fields' c
    doDecl (VariantType ty fields c) = do
        fields' <- mapM (doField ty) fields
        return $ VariantType ty fields' c
    doDecl d = return d

    doField enclT f = do
        t <- doType enclT (field_name f) (field_type f)
        return $ f { field_type = t }

    doType _ _ t@Prim{}     = return t
    doType _ _ t@Nominal{}  = return t
    doType enclT fn (Product fields) = do
        fields' <- mapM (doField enclT) fields
        let name = enclT <> "_" <> fn
        newName <- ensureUniqueTypeName name
        let topDecl = RecordType newName fields'
                            (Comment $ Just $ "A record type used inside " ++ enclT)
        tell [topDecl]
        return $ Nominal newName

    doType enclT fn (Sum fields) = do
        fields' <- mapM (doField enclT) fields
        let name = enclT <> "_" <> fn
        newName <- ensureUniqueTypeName name
        let topDecl = VariantType newName fields'
                            (Comment $ Just $ "A variant type used inside " ++ enclT)
        tell [topDecl]
        return $ Nominal newName

    doType enclT fn (Enum constrs) = do
        let name = enclT <> "_" <> fn
        newName <- ensureUniqueTypeName name
        let topDecl = EnumType newName constrs
                            (Comment $ Just $ "An enumerated type used inside " ++ enclT)
        tell [topDecl]
        return $ Nominal newName

    ensureUniqueTypeName name = do
        i <- lift $ gets (succ . Map.findWithDefault 0 name)
        modify (Map.insert name i)
        return $ if i==1 then name else name ++ "_" ++ show i

-- | If we have two or more fields with the same name, in the same record/variant, we
-- rename accordingly, taking care to maintain the field ordering.
ensureUniqueFieldNames :: [Decl a] -> [Decl a]
ensureUniqueFieldNames = map decl
  where
    decl (RecordType n fs c)   = RecordType n (fields fs) c
    decl (VariantType n fs  c) = VariantType n (fields fs) c
    decl d = d

    fields = map fst . List.sortOn snd -- recover ordering
           . concat . Map.elems -- ungroup
           . Map.map ensureUnique
           . Map.fromListWith (<>) . map (field_name . fst &&& (:[])) -- group
           . (`zip` [(1::Int)..]) -- record ordering

    ensureUnique fs | length fs > 1 = zipWith rename (List.sortOn snd fs) [(1::Int)..]
                    | otherwise     = fs

    rename (f, ord) i = (,ord) $ f { field_name = field_name f ++ show i }

-- | Used for a final rename after any re-arrangement and synthesis.
-- e.g. avoiding clashes with any Haskell keywords.
renameFields
    :: (Name -> Name)
    -> (Name -> Name)
    -> [Decl a]
    -> [Decl a]
renameFields recRename varRename = map decl
  where
    decl (RecordType n fs c)   = RecordType n (map (field recRename) fs) c
    decl (VariantType n fs  c) = VariantType n (map (field varRename) fs) c
    -- decl (ExtendType n b fs fsb c) = ExtendType n b (map (field recRename) fs)
    --                                                 (map (field recRename) fsb) c
    decl d = d
    field r f = f { field_name = r (field_name f)
                  , field_type = types (field_type f) }
    types n@Prim{} = n
    types n@Nominal{} = n
    types (Sum fs) = Sum $ map (field varRename) fs
    types (Product fs) = Product $ map (field recRename) fs
    types (Enum cs) = Enum $ map (\(name,meta,comment) -> (varRename name,meta,comment)) cs

-- This situation occurs a lot in FpML and makes for some annoying unnecessarily
-- nested types.
-- For example:
--     Sum [field "choice" (Sum fields), ...]
-- =>  Sum (fields ++ ...)
flattenNestedSums :: Eq a => (a -> Bool) -> [Decl a] -> [Decl a]
flattenNestedSums p = map decl
  where
    decl (RecordType n fs c)   = RecordType n (map field fs) c
    decl (VariantType n fs  c) = VariantType n (map field fs) c
    decl d = d

    field f = f { field_type = types (field_type f) }

    types (Sum fs) = Sum $ concatMap extract $ map field fs
    types (Product fs)  = Product $ map field fs
    types n = n

    extract (Field _ (Sum fs) card _ meta)
        | card == single
        , p meta = fs
    extract f    = [f]

-- | Is the type of the field a nominal type?
isNominal :: Field a -> Bool
isNominal (field_type -> Nominal{}) = True
isNominal _ = False

gatherReferencedDecls :: Map String (Decl a) -> Name -> Map String (Decl a)
gatherReferencedDecls allDecls topName =
  case Map.lookup topName allDecls of
      Nothing  -> mempty
      Just d   -> go mempty d
  where
    go m d
        | Just name <- getTypeName d
        , name `Map.notMember` m =
          let names = case d of
                  (RecordType    _ fs _) -> fields fs
                  (VariantType   _ fs _) -> fields fs
                  --(ExtendType    _ _  fs1 fs2 _) -> fields fs1 <> fields fs2
                  _ -> []
              deps = mapMaybe (\n -> Map.lookup n allDecls) names
          in List.foldl' go (Map.insert name d m) deps
        | otherwise = m

    fields fs = concatMap field fs
    field Field{..} = types field_type
    types (Prim _) = []
    types (Enum _) = []
    types (Nominal n) = [n]
    types (Sum fs) = fields fs
    types (Product fs) = fields fs
