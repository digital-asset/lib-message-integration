-- Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

-- | Convert XSD schema into our generic type model.
--
module DA.XML.Schema.Convert where

import           Control.Arrow             ((&&&))
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe
import           DA.Daml.TypeModel         (PrimType (..), Type (..))
import qualified DA.Daml.TypeModel         as Model
import           DA.XML.Schema.Parse       (mkQName, xsdNS)
import           DA.XML.Schema.Schema
import qualified Data.Char                 as Char
import           Data.List                 (foldl')
import           Data.List.Extra           (wordsBy)
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Maybe
import qualified Data.Set                  as Set
import           Data.Set                  (Set)
import qualified Data.Text                 as T
import           Data.Semigroup            (Semigroup, (<>))
import           Text.XML.Light            (QName (..))

type Conv = ReaderT Env (LoggingT IO)

-- | Metadata attached to each output field
data XmlMeta
    = XmlElement    QName
    | XmlElementRef QName -- refer to a top-level element, which may have substitutions
    | XmlAttribute  QName
    | XmlEnum       QName -- an enumeration
    | XmlContent   -- text simple content
    | XmlChoice    -- parser should use the variant type field meta for element names
    | XmlSequence  -- parser should use the record type field meta for element names
    | XmlAny
    deriving (Eq, Ord, Show)

data XmlTopLevelMeta n t = XmlTopLevelMeta
    { xmlNamespace      :: Maybe String
    , xmlSchemaVersion  :: Maybe String
    , xmlSchemaLocation :: Maybe String
    , xmlTopElemTypes   :: Map n t   -- top-level element names to types
    , xmlTopElemSubs    :: Map n [n] -- element substitution groups
    , xmlBaseTypes      :: Set t     -- non-abstract base types (variants)
    , xmlAbstractTypes  :: Set t     -- abstract types (variants)
    }

data Env = Env
    { env_type       :: Map QName (Either SimpleType ComplexType)
    , env_element    :: Map QName Element
    , env_attribute  :: Map QName Attribute
    , env_group      :: Map QName ModelGroup
    , env_attrGroup  :: Map QName AttributeGroup
    , env_extended   :: Map QName [QName] -- subtypes for supertype
    , env_substGrps  :: Map QName [QName] -- base, substitutable elems
    , env_typeToElem :: Map QName [QName] -- elements with this type
    , env_baseTy     :: Set QName         -- non-abstract base types that need to be variants
    }
    deriving (Show)

instance Monoid Env where
    mempty = Env mempty mempty mempty mempty mempty mempty mempty mempty mempty

instance Semigroup Env where
    (<>) e1 e2 = Env
        { env_type       = Map.union (env_type e1) (env_type e2)
        , env_element    = Map.union (env_element e1) (env_element e2)
        , env_attribute  = Map.union (env_attribute e1) (env_attribute e2)
        , env_group      = Map.union (env_group e1) (env_group e2)
        , env_attrGroup  = Map.union (env_attrGroup e1) (env_attrGroup e2)
        , env_extended   = Map.unionWith (<>) (env_extended e1) (env_extended e2)
        , env_substGrps  = Map.unionWith (<>) (env_substGrps e1) (env_substGrps e2)
        , env_typeToElem = Map.unionWith (<>) (env_typeToElem e1) (env_typeToElem e2)
        , env_baseTy     = Set.union (env_baseTy e1) (env_baseTy e2)
        }

mkEnv :: Schema -> Env
mkEnv Schema{..} = foldl' item mempty schema_items
  where
    item :: Env -> SchemaItem -> Env
    item env SchemaItem_Include      {} = env
    item env SchemaItem_Import       {} = env
    item env SchemaItem_Annotation   {} = env
    item env (SchemaItem_Component   c) = component env c

    component :: Env -> Component -> Env
    component env (Component_Attribute a)      = attribute env a
    component env (Component_Element e)        = element env e
    component env (Component_SimpleType s)     = simpleType env s
    component env (Component_ComplexType c)    = complexType env c
    component env (Component_ModelGroup g)     = modelGroup env g
    component env (Component_AttributeGroup g) = attrGroup env g
    component _ c = error $ "mkEnv: unsupported: "++ show c

    attribute :: Env -> Attribute -> Env
    attribute env a
        | Left (NameWithType name _) <- attribute_nameOrRef a =
              env { env_attribute = Map.insert (mkQName' name) a (env_attribute env) }
        | otherwise = env

    element :: Env -> Element -> Env
    element env e
        | Left (NameWithType name mty) <- element_nameOrRef e
               = (\env -> if element_abstract e
                      then env { env_baseTy =
                                 maybe id Set.insert mty (env_baseTy env) }
                      else env)
               . (\env ->
                      env { env_element =
                            Map.insert (mkQName' name) e (env_element env) })
               . (\env -> maybe env (\ty ->
                      env { env_typeToElem =
                            Map.insertWith (<>) ty [mkQName' name] (env_typeToElem env) }) mty)
               $ case element_substGroup e of
                  Nothing   -> env
                  Just base ->
                      env { env_substGrps =
                            Map.insertWith (<>) base [mkQName' name] (env_substGrps env)}
        | otherwise = env

    simpleType :: Env -> SimpleType -> Env
    simpleType env s
        | Just name <- simple_name s =
              env { env_type = Map.insert (mkQName' name) (Left s) (env_type env) }
        | otherwise = env

    complexType :: Env -> ComplexType -> Env
    complexType env c
        | Just name <- complex_name c =
          (\env -> env { env_type = Map.insert (mkQName name) (Right c) (env_type env) })
          $ case complex_content c >>= getExtensionBase of
                Just base -> env { env_extended =
                                   Map.insertWith (<>) base [mkQName name] (env_extended env) }
                Nothing -> env
        | otherwise = env
       where
         getExtensionBase (ComplexContent e _) = either (const Nothing) extension_base e
         getExtensionBase (SimpleContent e) = either (const Nothing)  extension_base e
         getExtensionBase OtherContent{} = Nothing

    modelGroup :: Env -> ModelGroup -> Env
    modelGroup env g
        | Left name <- modelGroup_nameOrRef g =
              env { env_group = Map.insert (mkQName' name) g (env_group env) }
        | otherwise = error $ show g

    attrGroup :: Env -> AttributeGroup -> Env
    attrGroup env g
        | Left name <- attributeGroup_nameOrRef g =
              env { env_attrGroup = Map.insert (mkQName' name) g (env_attrGroup env) }
        | otherwise = error $ show g

    mkQName' = mkQName . last . wordsBy (==':')


-- | Find all direct module dependencies.
gatherImports :: Schema -> [(FilePath, Maybe String)]
gatherImports s =
    [ (include_schemaLocation, Nothing)
    | (SchemaItem_Include Include{..}) <- schema_items s
    ] -- ++
    -- NOTE: only fpml-msg-5-10.xsd currently uses import:
    -- <xsd:import namespace="http://www.w3.org/2000/09/xmldsig#" schemaLocation="xmldsig-core-schema.xsd" />
    -- it contains qnames with a "sig" prefix.
    -- [ (import_schemaLocation, ns)
    -- | (SchemaItem_Import  Import{..}) <- schema_items s
    -- , let ns = targetPrefix (Just import_namespace) (schema_namespaces s)
    -- ]

-- | Metadata describing top-level elements and types.
mkXmlTopLevelMeta :: Env -> Schema -> XmlTopLevelMeta QName (Type XmlMeta)
mkXmlTopLevelMeta Env{..} Schema{..} =
    XmlTopLevelMeta
    { xmlNamespace      = schema_targetNamespace
    , xmlSchemaVersion  = schema_version
    , xmlSchemaLocation = schema_schemaLocation
    , xmlTopElemTypes   = fmap mkFieldType elemTyMap
    , xmlTopElemSubs    = env_substGrps
    , xmlBaseTypes      = Set.map mkFieldType env_baseTy
    , xmlAbstractTypes  = Set.map mkFieldType . Map.keysSet . Map.filter isAbstract $ env_type
    }
  where
    elemNames  = Map.keys env_element
    elemTyMap  = Map.fromList [(name, fromJust mty)
                              | name <- elemNames
                              , let mty = elemTy name
                              , isJust mty
                              ]

    elemTy n = Map.lookup n env_element >>= getElementType

    isAbstract = either (const False) complex_abstract

-- | Retrieves the nominal type given to this element, if any.
getElementType :: Element -> Maybe QName
getElementType
    = either (\(NameWithType _ ty) -> ty) (const Nothing)
    . element_nameOrRef

convert :: Name -> Schema -> Conv (Model.Module XmlMeta)
convert name Schema{..} = do
    decls <- fmap
           ( Model.ensureUniqueFieldNames
           . Model.renameFields renameRecField renameVarField
           . Model.typeLift
           . Model.flattenNestedSums (==XmlChoice)
           . concat)
           $ mapM convItem schema_items

    return Model.Module
        { module_name      = name    -- fully qualified
        , module_imports   = []
        , module_decls     = decls
        , module_comment   = Model.noComment -- TODO grab the XML comment
        }

convItem :: SchemaItem -> Conv [Model.Decl XmlMeta]
convItem (SchemaItem_Include      Include{..}) = return []
convItem (SchemaItem_Import       Import{..})  = return []
convItem (SchemaItem_Annotation   ann) = return
    [Model.InlineComment (unAnnotation ann)]
convItem (SchemaItem_Component    c) = convComponent c

convComponent :: Component -> Conv [Model.Decl XmlMeta]
convComponent (Component_Attribute _)      = return [] -- Not needed for FpML
convComponent (Component_Element e)        = convElement e
convComponent (Component_SimpleType s)     = convSimpleType (error "convSimpleType: missing name") s
convComponent (Component_ComplexType c)    = convComplexType (error "convComplexType: missing name") c
convComponent (Component_ModelGroup _)     = return [] -- Handled by environment
convComponent (Component_AttributeGroup _) = return [] -- Handled by environment
convComponent c                            = error $ "Schema component not supported: " ++ show c

convElement :: Element -> Conv [Model.Decl XmlMeta]
convElement Element{..} =
    case element_nameOrRef of
        Left (NameWithType name mty) -> do
             case element_content of
                 Just content ->
                     either (convSimpleType name) (convComplexType name) content
                 Nothing
                     | Just ty@Prim{} <- mkFieldType <$> mty ->
                       return [ Model.NewType (convTypeName name) ty (mkComment element_annotation) ]
                     | otherwise -> return [] -- Handled by environment
        Right _ref -> return [] -- Handled by environment

convSimpleType :: Name -> SimpleType -> Conv [Model.Decl XmlMeta]
convSimpleType defName s = do
    me  <- runMaybeT (isEnumeration s)
    case me of
      Just enums -> do
          let name   = fromMaybe defName $ simple_name s
              enums' = map (\(name, comment) ->
                                (convFieldName name, XmlEnum (mkQName name), comment)) enums
          return [ Model.EnumType (convTypeName name) enums' (mkComment $ simple_annotation s) ]
      Nothing ->
          case simple_item s of
              Restricted (Restriction ann (Just base) (Nothing, _facets)) -> do
                   let name    = fromMaybe defName $ simple_name s
                       comment = mkComment $ (simple_annotation s) <> ann
                   return [ Model.NewType (convTypeName name) (mkFieldType base) comment ]
              Restricted r  -> error $ "convSimpleType: unsupported: "++ show r
              UnionOf as es -> do
                  let name    = fromMaybe defName $ simple_name s
                      comment = mkComment $ simple_annotation s
                  ts     <- mapM lookupSimpleType' as
                  fields <- concat <$> mapM mkFieldsFromSimpleType (ts ++ es)
                  -- TODO we should omit the enum fields first to make sure
                  -- we parse into enums in preference to primitives.
                  return [ Model.VariantType (convTypeName name) fields comment ]
              ListOf     e  -> error $ "convSimpleType: unsupported: "++ show e

  where
      lookupSimpleType' n =
          fromMaybe (error $ "Could not find simpleType: " <> show n)
              <$> runMaybeT (lookupSimpleType n)

-- | complex types can and often are named, so we convert them into declarations.
convComplexType :: Name -> ComplexType -> Conv [Model.Decl XmlMeta]
convComplexType defName c
    -- we require any complex type that are used for abstract elements to be
    -- encoded as a variant type. If the type itself is not declared as abstract,
    -- then we must generate an additional type for "basetype" instances.
    | complex_abstract c = do
          -- NOTE: e.g. UnderlyingAsset is abstract *and* has complex content.
          -- It should become a sum type, with all concrete members getting its fields
          -- We use the env for this, here we just generate the Variant, for the subtypes
          -- we can lookup the base fields using the environment (env_type).
          alts <- getAlternatives (mkQName name)
          return [Model.VariantType (convTypeName name) alts comment]
    | otherwise = do
          fields <- mkFieldsFromComplexType c
          -- Is this type used for abstract elements with substitution groups?
          requireVariant <- asks $ Set.member (mkQName name) . env_baseTy
          if requireVariant
              then do
                  let baseType = convTypeName name
                  -- Since we have a non-abstract base type, we will
                  -- need to create a separate type for its contents.
                  -- TODO the contents may be empty (e.g. FxDisruptionEvent),
                  -- should we special case it?
                  alts <- getAlternatives (mkQName name)
                  return [ Model.VariantType baseType alts comment
                         , Model.RecordType (mkContentsType baseType) fields
                             (Model.Comment . Just $ "Contents for base type "++ baseType)
                         ]

              else return $ case fields of
                -- record with a single-field choice type, should be a variant
                [Model.Field _ (Sum fields') card _ XmlChoice] | card == Model.single
                  -> [Model.VariantType (convTypeName name) fields' comment]
                _ -> [Model.RecordType (convTypeName name) fields comment]
  where

      name = fromMaybe defName $ complex_name c
      comment = mkComment $ complex_annotation c

      mkContentsType :: Name -> Name
      mkContentsType baseType = baseType ++ "Contents"

      -- Alternatives are based on subtypes that can be substituted using the "xsi:type"
      -- attribute and elements that can be substituted using substitution groups.
      getAlternatives :: QName -> Conv [Model.Field XmlMeta]
      getAlternatives ty = do
          elemSubs <- map (mkAlternative ty) <$> getSubstitutions ty
          tySubs   <- map (mkAlternative ty . (id &&& id)) <$> getSubTypes ty
          -- do not include a subtype field if we already have one from
          -- a substitution group.
          let tys = Set.fromList (map Model.field_type elemSubs)
          return $ elemSubs ++ filter ((`Set.notMember` tys) . Model.field_type) tySubs

      mkAlternative :: QName -> (QName, QName) -> Model.Field XmlMeta
      mkAlternative baseTy (name, ty) =
          let ty' | baseTy==ty = mkQName (mkContentsType $ qName ty)
                  | otherwise  = ty
          in Model.Field
          { field_name        = convFieldName (qName name)
          , field_type        = mkFieldType ty'
          , field_cardinality = Model.single
          , field_comment     = Model.noComment
          , field_meta        = XmlElement name
          }

      -- Given a type name, return all the element substitions and their subtypes.
      --
      -- An element may be the base of a "substitution group", which allows any other
      -- element in the group to be substituted. This is yet another mechanism for choice
      -- and we try to encode this as alternatives in the corresponding base type.
      getSubstitutions :: QName -> Conv [(QName, QName)]
      getSubstitutions ty = do
          -- all the elements that have this type
          elemNames  <- fromMaybe [] <$> lookupFrom env_typeToElem ty
          -- all the substitutions
          subsNames <- (concatMap $ fromMaybe []) <$> mapM (lookupFrom env_substGrps) elemNames
          subsElems <- (map $ fromMaybe err1) <$> mapM (lookupFrom env_element) subsNames
          return
              [ (name, ty)
              | (name, elem) <- zip subsNames subsElems
              , let ty = fromMaybe err2 (getElementType elem)
              , not (element_abstract elem)
              ]
        where
          err1 = error "getSubstitutions: assertion failed: missing element definition"
          err2 = error "getSubstitutions: assertion failed: missing element type"

      -- Given a type name, return all the 'subtypes'.
      -- These must be added to any base types as XML allows substitution
      -- of any base type with a subtype by specifying the type on the
      -- element name, e.g.
      -- <paymentRule xsi:type="PercentageRule">...</paymentRule>
      --
      -- NOTE: This is recursive. We collect all the transitive subtypes
      -- and flatten the hierarchy, this is consistent with inlining all supertype
      -- fields into records, instead of using composition. The result is smaller
      -- and simpler DAML-LF values; at the expense of larger types.
      getSubTypes :: QName -> Conv [QName]
      getSubTypes ty = do
          requireVariant <- asks $ Set.member ty . env_baseTy
          isAbstract <- getAbstractFlag <$> lookupFrom env_type ty
          mtys <- lookupFrom env_extended ty
          case mtys of
              Just tys -> do
                  tys' <- concat <$> mapM getSubTypes tys
                  return $
                      if requireVariant || isAbstract
                      then tys'
                      else ty : tys'
              Nothing
                  | requireVariant || isAbstract -> return []
                  | otherwise -> return [ty]
        where
          getAbstractFlag :: Maybe (Either SimpleType ComplexType) -> Bool
          getAbstractFlag = maybe False (either (const False) complex_abstract)


mkFields :: Either Elements Attributes -> Conv [Model.Field XmlMeta]
mkFields = either mkFieldsFromElems mkFieldsFromAttrs

-- NB: We need to inline sequence elements as they would appear in a conforming
-- XML file.
mkFieldsFromElems :: Elements -> Conv [Model.Field XmlMeta]
mkFieldsFromElems (Elements_Choice ann occurs items) = do
    fieldss <- mapM mkFieldsFromElems items
    return $ case map mkAlternative fieldss of
        [f] | card == Model.single -> [f] -- eliminate single choice field
        fs -> [Model.Field "choice" (Model.Sum fs) card (mkComment ann) XmlChoice]
  where
    mkAlternative [field] = field
    mkAlternative fs      =
        Model.Field "items" (Model.Product fs)
                            Model.single (Model.Comment Nothing) XmlSequence
    card = convOccurs occurs

mkFieldsFromElems (Elements_Sequence ann occurs items) = do
    fields <- concat
          <$> mapM mkFieldsFromElems items
    return $ inlineSequenceIfSingle "sequence" occurs ann fields

mkFieldsFromElems (Elements_Element e) = do
    case element_nameOrRef e of
        Left (NameWithType name mtype) -> do
            ty <- case mtype of
                      Just ty -> return $ mkFieldType ty
                      Nothing -> do
                          fields <- maybe (return []) mkFieldsFromContent (element_content e)
                          -- avoid creating a product type with a single field of choice type
                          return $ case fields of
                              [Model.Field _ (ty@Sum{}) card _ XmlChoice]
                                  | card == Model.single -> ty
                              _ -> Model.Product fields

            return [Model.Field (convFieldName name) ty (convOccurs (element_occurs e))
                                (mkComment $ element_annotation e)
                                (XmlElement $ mkQName name)]

        -- NOTE: we avoid just inlining element-refs here, otherwise
        -- we may end up with potentially many instances of a particular
        -- anonymous structural type.
        Right ref -> do
            m <- lookupFrom env_element ref
            case m of
                Just e'  -> do
                    [f] <- map (Model.applyCard $ convOccurs (element_occurs e)) <$>
                               mkFieldsFromElems (Elements_Element e')
                    let ty = case Model.field_type f of
                                 ty@Model.Nominal{} -> ty
                                 -- assume a nominal type was created with the same name as the original element
                                 _ -> case element_nameOrRef e' of
                                          Left (NameWithType name _mty) -> Model.Nominal (convTypeName name)
                                          Right ref -> error $ "Unsupported chain of element references: " ++ show ref
                    return [f { Model.field_type = ty
                              , Model.field_meta = XmlElementRef ref
                              }]
                -- TODO support namespaces, for e.g. "dsig" elements
                Nothing | qPrefix ref == Just "dsig" -> return [] -- Ignore dsig namespace
                        | otherwise -> error $ "Cannot find element reference: " ++ show ref

mkFieldsFromElems (Elements_Group ModelGroup{..}) =
    case modelGroup_nameOrRef of
        Left name -> do
            fields <- maybe (return []) mkFieldsFromElems modelGroup_content
            return $ inlineSequenceIfSingle (convFieldName name)
                                   modelGroup_occurs
                                   modelGroup_annotation
                                   fields
        Right ref -> do
            m <- lookupFrom env_group ref
            case m of
                Just g  -> do
                    fields <- mkFieldsFromElems (Elements_Group g)
                    return $ inlineSequenceIfSingle (convFieldName $ qName ref)
                                           modelGroup_occurs
                                           modelGroup_annotation fields
                Nothing -> do -- TODO errors
                    logWarnN $ "Skipping " <> T.pack (show ref)
                    return []
mkFieldsFromElems Elements_Any          =
    -- Due to currently no support for ExistentialTypes in DAML, we support xsd:any using a text type.
    return [Model.Field "any" (Prim PrimText) Model.single Model.noComment XmlAny]

mkFieldsFromElems e@Elements_All{}      = error $ "mkFieldsFromElems: unsupported: " ++ show e

mkFieldsFromAttrs :: Attributes -> Conv [Model.Field XmlMeta]
mkFieldsFromAttrs = either (fmap concat . mapM mkFieldsFromAttr) mkFieldsFromAttrGroup

mkFieldsFromAttr :: Attribute -> Conv [Model.Field XmlMeta]
mkFieldsFromAttr Attribute{..} = do
    case attribute_nameOrRef of
        Left (NameWithType name mtype) -> do
            let ty   = maybe defaultAttrTy mkFieldType mtype
                -- default to text, if no type specified (e.g. href)
                defaultAttrTy = Prim PrimText
            return [Model.Field (convFieldName name) ty (convUse attribute_use)
                                (mkComment attribute_annotation)
                                (XmlAttribute $ mkQName name)]
        Right ref -> do
            m <- lookupFrom env_attribute ref
            case m of
                Just a  ->
                    map (Model.applyCard $ convUse attribute_use)
                    <$> mkFieldsFromAttr a
                Nothing -> error $ "Unknown attribute reference: " ++ show ref

mkFieldsFromAttrGroup :: AttributeGroup -> Conv [Model.Field XmlMeta]
mkFieldsFromAttrGroup AttributeGroup{..} =
    case attributeGroup_nameOrRef of
        Left _name -> concat
            <$> mapM mkFieldsFromAttrs attributeGroup_content
        Right ref  -> do
            m <- lookupFrom env_attrGroup ref
            case m of
                Just g  -> mkFieldsFromAttrGroup g
                Nothing -> error $ "Unknown attribute group reference: " ++ show ref

mkFieldsFromContent
    :: Either SimpleType ComplexType
    -> Conv [Model.Field XmlMeta]
mkFieldsFromContent =
    either mkFieldsFromSimpleType mkFieldsFromComplexType

mkFieldsFromSimpleType
    :: SimpleType
    -> Conv [Model.Field XmlMeta]
mkFieldsFromSimpleType s@SimpleType{..} = do
    me  <- runMaybeT (isEnumeration s)
    case me of
      Just enums
          | Just name <- simple_name -> do
                return [ Model.Field "enum" (Nominal name) Model.single
                             (mkComment simple_annotation) XmlChoice ]
          | otherwise -> do
                let enumTy = Model.Enum $ map (\(name, comment) ->
                                 (convFieldName name, XmlEnum (mkQName name), comment)) enums
                return [ Model.Field "enum" enumTy Model.single
                             (mkComment simple_annotation) XmlChoice ]
      Nothing ->
          case simple_item of
              Restricted (Restriction ann (Just base) _) -> do
                   let name = fromMaybe "restriction" simple_name
                   let comment = mkComment $ simple_annotation <> ann
                   return [ Model.Field (convTypeName name) (mkFieldType base) Model.single comment XmlChoice ]
              Restricted r  ->
                  error $ "mkFieldsFromSimpleType: Restricted with no base type unsupported: "++ show r
              UnionOf as es ->
                  error $ "mkFieldsFromSimpleType: Union unsupported: "++ show (as, es)
              ListOf     e  ->
                  error $ "mkFieldsFromSimpleType: List unsupported: "++ show e

mkFieldsFromComplexType
    :: ComplexType
    -> Conv [Model.Field XmlMeta]
mkFieldsFromComplexType ComplexType{..} =
    case complex_content of
        Just (ComplexContent item _mixed) ->
            case item of
                Left Restriction{}  ->
                    error $ "mkFieldsFromComplexType: Restriction unsupported: "++ show item
                Right Extension{..} -> do
                    fields      <- maybe (return []) mkFieldsFromOtherContent extension_type
                    base_fields <- maybe (return []) mkFieldsFromTypeName extension_base
                    return $ base_fields <> fields

        Just (SimpleContent item) ->
            case item of
                Left Restriction{}  ->
                    error $ "mkFieldsFromComplexType: Restriction unsupported: "++ show item
                Right Extension{..} -> do
                    content     <- maybe (return []) mkFieldsFromSimpleContentBase extension_base
                    attr_fields <- maybe (return []) mkFieldsFromAttrs extension_type
                    return $ attr_fields <> content

        Just (OtherContent c) -> mkFieldsFromOtherContent c
        Nothing -> return []
  where
    mkFieldsFromSimpleContentBase tyName = do
        fields <- mkFieldsFromTypeName tyName
        let ty = mkFieldType tyName
        let contentField = [Model.Field "content" ty Model.single Model.noComment XmlContent]
        -- TODO we need to improve the logic around identifying the content field
        -- The types of the content field may be text or enums, but here we see
        -- extended types such as "NormalizedString" in FpML.
        case fields of
            [Model.Field _ _ card _ _] | card==Model.single -> return contentField
            _ | Prim{} <- ty -> return contentField
              | otherwise    -> return fields -- assume an extended simpletype

mkFieldsFromOtherContent
    :: (Maybe Elements, [Attributes], Maybe AnyAttr)
    -> Conv [Model.Field XmlMeta]
mkFieldsFromOtherContent (_, _, Just{}) =
    error "anyAttribute currently unsupported"
mkFieldsFromOtherContent (melems, attrs, Nothing) =
    (++) <$> (concat <$> mapM mkFieldsFromAttrs attrs)
         <*> maybe (return []) mkFieldsFromElems melems

mkFieldsFromTypeName :: QName -> Conv [Model.Field XmlMeta]
mkFieldsFromTypeName name = do
    m <- lookupFrom env_type name
    case m of
        Nothing   -> return []
        Just cont -> mkFieldsFromContent cont

isEnumeration :: SimpleType -> MaybeT Conv [(Name, Model.Comment)]
isEnumeration SimpleType{..} =
    case simple_item of
        Restricted (Restriction _ _ (_, facets)) -> do
            let enum = [ (facet_value, mkComment facet_annotation)
                       | Facet{..} <- facets
                       , facet_kind == Facet_Enumeration
                       ]
            if null enum
               then mzero
               else return enum
        UnionOf attrs elems -> do
            attrs_tys <- mapM lookupSimpleType attrs
            concat <$> mapM isEnumeration (attrs_tys ++ elems)
        ListOf{} -> mzero

lookupSimpleType :: QName -> MaybeT Conv SimpleType
lookupSimpleType qn = MaybeT $ do
    ty <- lookupFrom env_type qn
    case ty of
        Just (Left st) -> return $ Just st
        _ -> return Nothing

-- It is only safe to inline sequences if they have one-to-one
-- cardinality.  Otherwise, we will have parsing ambiguities.
inlineSequenceIfSingle
    :: Name
    -> Occurs
    -> Maybe Annotation
    -> [Model.Field XmlMeta]
    -> [Model.Field XmlMeta]
inlineSequenceIfSingle name occurs ann fields =
    case convOccurs occurs of
        -- safe to inline
        c | c == Model.single  -> fields
        -- avoid creating a product type with a single field of choice type
          | [Model.Field _ ty@Sum{} c' _ XmlChoice] <- fields
          , c' == Model.single ->
                [Model.Field name ty c (mkComment ann) XmlChoice]
        -- need to create a composite field
          | otherwise ->
                [Model.Field name (Model.Product fields) c (mkComment ann) XmlSequence]

lookupFrom
    :: (MonadReader Env m, Ord k)
    => (Env -> Map k e)
    -> k
    -> m (Maybe e)
lookupFrom f k = do
    env <- asks f
    return $ Map.lookup k env

-- In the future, we could output newtypes for many of these, but for now
-- we just map them to primitive underlying types.
mkFieldType :: QName -> Type XmlMeta
mkFieldType QName{..} =
    case qName of
        -- The xsd:any type, which we do not support (requires ExistentialTypes in DAML).
        "any"                | isXsd -> Prim PrimText

        "decimal"            | isXsd -> Prim PrimDecimal
        "integer"            | isXsd -> Prim PrimInteger
        "int"                | isXsd -> Prim PrimInteger
        "nonNegativeInteger" | isXsd -> Prim PrimInteger
        "positiveInteger"    | isXsd -> Prim PrimInteger
        "boolean"            | isXsd -> Prim PrimBool
        "string"             | isXsd -> Prim PrimText
        "date"               | isXsd -> Prim PrimDate
        "dateTime"           | isXsd -> Prim PrimTime
        "href"               | isXsd -> Prim PrimText
        "normalizedString"   | isXsd -> Prim PrimText
        "token"              | isXsd -> Prim PrimText
        "ID"                 | isXsd -> Prim PrimText
        "IDREF"              | isXsd -> Prim PrimText
        "anyURI"             | isXsd -> Prim PrimText
        "hexBinary"          | isXsd -> Prim PrimText
        "base64Binary"       | isXsd -> Prim PrimText
        "language"           | isXsd -> Prim PrimText

        -- These temporal XSD types are not currently mapped to any DAML temporal
        -- types, but instead left as strings, for now.
        "time"               | isXsd -> Prim PrimText
        "gYearMonth"         | isXsd -> Prim PrimText
        "gYear"              | isXsd -> Prim PrimText
        "gMonthDay"          | isXsd -> Prim PrimText
        "gDay"               | isXsd -> Prim PrimText
        "gMonth"             | isXsd -> Prim PrimText

        name                         -> Nominal $ convTypeName name
  where
    isXsd = qURI == Just xsdNS

-- This should never be a primitive type, only
-- an existing nominal type.
convTypeQName :: QName -> Name
convTypeQName q@QName{..} =
    case (qPrefix, qName) of
        (Just{},  _ ) -> error $ "convQTypeName: assertion failed: unexpected type: " ++ show q
        (Nothing, ty) -> convTypeName ty

-- Force the first letter to be capitalised and
-- encode any illegal chars. Also map a few name clashes.
convTypeName :: Name -> Name
convTypeName "Unit"       = "FpmlUnit"
convTypeName "Party"      = "FpmlParty"
convTypeName "ContractId" = "FpmlContractId"
convTypeName (c:cs)
    | Char.isLower c      = encodeChars $ Char.toUpper c : cs
convTypeName n = encodeChars n

-- During conversion, just encode illegal characters.
-- More work will be needed later.
convFieldName :: Name -> Name
convFieldName = encodeChars

renameRecField :: Name -> Name
renameRecField (c:cs) = avoidKeywords $ Char.toLower c : cs
  where
    avoidKeywords n | n `Set.member` keywords = n ++ "_" -- append underscore
                    | otherwise = n
    keywords = Set.fromList ["choice","sequence","any","time","date","type","id","dayOfWeek"]
renameRecField _ = error "renameRecField: unexpected single character name"

renameVarField :: Name -> Name
renameVarField (c:cs) = avoidKeywords $ Char.toUpper c : cs
  where
    avoidKeywords n | n `Set.member` keywords = n ++ "_" -- append underscore
                    | otherwise = n
    keywords = Set.fromList ["Choice"]
renameVarField _ = error "renameVarField: unexpected single character name"

-- NOTE: Turn the following chars into underscores:
-- ' ', ':', '/', '\\', '.', '(', ')'
encodeChars :: String -> String
encodeChars = map replace
  where
    replace c | c `Set.member` invalid = '_'
              | otherwise = c

    invalid = Set.fromList [' ', ':', '/', '\\', '.', '(', ')', '-', '?']

convOccurs :: Occurs -> Model.Cardinality
convOccurs (Occurs 0 (Bounded 1)) = Model.optional
convOccurs (Occurs 1 (Bounded 1)) = Model.single
convOccurs (Occurs 0 _)           = Model.many
convOccurs _                      = Model.many1

convUse :: Use -> Model.Cardinality
convUse Required   = Model.single
convUse Optional   = Model.optional
convUse Prohibited = error "mkFieldsFromAttrs: Prohibited not supported"

mkComment :: Maybe Annotation -> Model.Comment
mkComment (Just (Annotation s)) | not (null s) = Model.Comment (Just s)
mkComment _ = Model.noComment

-- | Find the supertype (if it exists) of a given type name.
-- NOTE: not currently used, since we flatten variant hierarchies.
superTypeOf :: Env -> QName -> Maybe QName
superTypeOf env name = do
    cont <- Map.lookup name (env_type env)
    item <- either (const Nothing) complex_content cont
    case item of
        ComplexContent cont _ -> either (const Nothing) extension_base cont
        SimpleContent cont    -> either (const Nothing) extension_base cont
        OtherContent{}        -> Nothing
