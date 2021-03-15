-- Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

module DA.Daml.GenDaml where

import           Prelude hiding ((<>))
import           DA.Daml.TypeModel
import           Text.PrettyPrint.HughesPJ as PP

import qualified Data.List as List
import qualified Data.Set as Set

-- | Convert from the TypeModel to a DAML source text.
ppModule :: Show a => Module a -> Doc
ppModule Module{..} =
    let primitiveTypes = List.foldl' usedPrimitives Set.empty module_decls
        primitiveImports = List.intercalate ", " $ map ppPrimTypeText $ Set.elems primitiveTypes
    in
    text "-- Generated by Metagen"
        $$ text "module" <+> text module_name
        $$ nest 2 (text "( module" <+> text module_name <+> text ") where")
        $$ ppImports module_imports
        $$ text "import Prelude ( Eq, Ord, Show, Optional, " <> text primitiveImports <> text " )"   -- Detecting Optional is a bit more onerous
        $$ text ""
        $$ ppDecls module_decls

ppImports :: [Import] -> Doc
ppImports = vcat . List.intersperse (text "") . map ppImport
  where
    ppImport (Unqualified modu) =
        text "import" <+> text modu
    ppImport (Qualified modu prefix) =
        text "import qualified" <+> text modu <+> text "as" <+> text prefix

ppDecls :: Show a => [Decl a] -> Doc
ppDecls = vcat . List.intersperse (text "") . map ppDecl

ppDecl :: Show a => Decl a -> Doc

ppDecl (EnumType name constrs comment) =
    ppComment Before comment
        $$ text "data" <+> text name
        $$ nest 4 (ppBlock "=" "|" "deriving (Eq, Ord, Show)" ppConstr constrs)
  where
    ppConstr (cname, _, comment')
        = (text name <> text "_" <> text cname)
              $$ ppComment After comment'

ppDecl (RecordType name fields comment) =
    ppComment Before comment
    $$ text "data" <+> text name <+> text "=" <+> text name <+> text "with"
        $$ nest 4 (ppRecordFields fields
                  $$ nest 2 (text "deriving (Eq, Ord, Show)"))

ppDecl (VariantType name fields comment)
    | not (null fields) =
      ppComment Before comment
      $$ text "data" <+> text name
          $$ nest 4 (ppBlock "=" "|" "deriving (Eq, Ord, Show)" ppAlternative fields)
    | otherwise =
      -- TODO empty variant, should be an error!
      ppComment Before comment
      $$ text "data" <+> text name <+> text "=" <+> text name <+> text "()"
          $$ nest 4 (text "deriving (Eq, Ord, Show)")
  where
    ppAlternative Field{..} =
        text name <> text "_" <> text field_name <+> ppType field_cardinality parens field_type
            $$ ppComment After field_comment

-- TODO The DAML-LF story on these is not yet clear.
-- For now, emit as type synonyms
ppDecl (NewType name base comment) =
    ppComment Before comment
    {-- $$ text "newtype" <+> text name <+> text "=" <+> text name <+> ppType single id base
    --     $$ nest 4 (text "deriving (Eq, Ord, Show)") -}
    $$ text "type" <+> text name <+> text "=" <+> ppType single id base

ppDecl d = text $ "-- UNSUPPORTED: " ++ show d

ppRecordFields :: [Field a] -> Doc
ppRecordFields fs
    | null fs   = empty
    | otherwise = vcat $ map ppField fs
  where
    ppField :: Field a -> Doc
    ppField Field{..} =
        text field_name <+> text ":" <+> ppType field_cardinality id field_type
            $$ nest 4 (ppComment After field_comment)

ppType :: Cardinality -> (Doc -> Doc) -> Type a -> Doc
ppType c k (Prim prim)   =
    applyCardinality c k $ ppPrimType prim
ppType c k (Nominal name)   =
    applyCardinality c k $ text name
ppType c k (Product fields) =
    applyCardinality c k $ ppTuple fields
ppType _ _ Enum    {}       = error "Anonymous enum types not currently supported"
ppType _ _ Sum     {}       = error "Anonymous sum types not currently supported"

ppPrimTypeText :: PrimType -> String
ppPrimTypeText PrimText     = "Text"
ppPrimTypeText PrimBool     = "Bool"
ppPrimTypeText PrimInteger  = "Int"
ppPrimTypeText PrimDecimal  = "Decimal"
ppPrimTypeText PrimTime     = "Time"
ppPrimTypeText PrimDate     = "Date"
ppPrimTypeText PrimUnit     = "Unit"
ppPrimTypeText PrimParty    = "Party"


ppPrimType :: PrimType -> Doc
ppPrimType = text . ppPrimTypeText

-- The only structural type we choose to support.
ppTuple :: [Field a] -> Doc
ppTuple =
    parens
    . sep
    . List.intersperse (text ",")
    . map (\Field{..} -> ppType field_cardinality id field_type)

applyCardinality :: Cardinality -> (Doc -> Doc) -> Doc -> Doc
applyCardinality (Cardinality One ToOne)   _ d = d
applyCardinality (Cardinality Zero ToOne)  k d = k $ text "Optional" <+> k d
applyCardinality (Cardinality _ ToMany)    _ d = brackets d

ppBlock :: String -> String -> String -> (a->Doc) -> [a] -> Doc
ppBlock open _sep close _pp []   =
    text open <> text close
ppBlock open sep close pp (x:xs) =
    text open <+> pp x
        $$ vcat (map (\y-> text sep <+> pp y) xs)
        $$ text close

data CommentPosition = Before | After

ppComment :: CommentPosition -> Comment -> Doc
ppComment _   (Comment Nothing)  = empty
ppComment pos (Comment (Just s))
    | null s    = empty
    | otherwise =
      text "--" <+> text (case pos of Before -> "|"; After -> "^") <+> text c
      $$ vcat (map (\x-> text "--  " <+> text x) cs)
  where
    (c:cs) = lines (paragraphFill 80 s)

paragraphFill :: Int -> String -> String
paragraphFill lineLen =
    render' . fsep . map text . words
  where
    render' = renderStyle (Style PageMode lineLen 1.5)
