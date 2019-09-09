module DA.CDM.Rosetta.Static where

import qualified DA.Daml.TypeModel     as Model
import           DA.CDM.Rosetta.Convert (Conv, CdmMeta(..))

moduleMetaClasses :: String -> Conv (Model.Module CdmMeta)
moduleMetaClasses moduleName = return Model.Module
      { module_name = moduleName
      , module_imports = []
      , module_decls =
        [ Model.HigherRecordType
            "ReferenceWithMeta"
            ["a"]
            [ Model.Field
                { field_name = "globalReference"
                , field_type = Model.Prim Model.PrimText
                , field_cardinality = Model.optional
                , field_comment = Model.noComment
                , field_meta = CdmField "globalReference" Nothing
                }
            , Model.Field
                { field_name = "externalReference"
                , field_type = Model.Prim Model.PrimText
                , field_cardinality = Model.optional
                , field_comment = Model.noComment
                , field_meta = CdmField "externalReference" Nothing
                }
            , Model.Field
                { field_name = "value"
                , field_type = Model.Nominal "a"
                , field_cardinality = Model.optional
                , field_comment = Model.noComment
                , field_meta = CdmField "value" Nothing
                }
            , Model.Field
                { field_name = "meta"
                , field_type = Model.Nominal "MetaFields"
                , field_cardinality = Model.optional
                , field_comment = Model.noComment
                , field_meta = CdmField "meta" Nothing
                }
            ]
            Model.noComment
        , Model.HigherRecordType
            "BasicReferenceWithMeta"
            ["a"]
            [ Model.Field
                { field_name = "globalReference"
                , field_type = Model.Prim Model.PrimText
                , field_cardinality = Model.optional
                , field_comment = Model.noComment
                , field_meta = CdmField "globalReference" Nothing
                }
            , Model.Field
                { field_name = "externalReference"
                , field_type = Model.Prim Model.PrimText
                , field_cardinality = Model.optional
                , field_comment = Model.noComment
                , field_meta = CdmField "externalReference" Nothing
                }
            , Model.Field
                { field_name = "value"
                , field_type = Model.Nominal "a"
                , field_cardinality = Model.optional
                , field_comment = Model.noComment
                , field_meta = CdmField "value" Nothing
                }
            , Model.Field
                { field_name = "meta"
                , field_type = Model.Nominal "MetaFields"
                , field_cardinality = Model.optional
                , field_comment = Model.noComment
                , field_meta = CdmField "meta" Nothing
                }
            ]
            Model.noComment
        , Model.HigherRecordType
            "FieldWithMeta"
            ["a"]
            [ Model.Field
                { field_name = "value"
                , field_type = Model.Nominal "a"
                , field_cardinality = Model.single
                , field_comment = Model.noComment
                , field_meta = CdmField "value" Nothing
                }
            , Model.Field
                { field_name = "meta"
                , field_type = Model.Nominal "MetaFields"
                , field_cardinality = Model.optional
                , field_comment = Model.noComment
                , field_meta = CdmField "meta" Nothing
                }
            ]
            Model.noComment
        , Model.RecordType
            "MetaFields"
            [ Model.Field
                { field_name = "reference"
                , field_type = Model.Prim Model.PrimText
                , field_cardinality = Model.optional
                , field_comment = Model.noComment
                , field_meta = CdmField "reference" Nothing
                }
            , Model.Field
                { field_name = "scheme"
                , field_type = Model.Prim Model.PrimText
                , field_cardinality = Model.optional
                , field_comment = Model.noComment
                , field_meta = CdmField "scheme" Nothing
                }
            , Model.Field
                { field_name = "id"
                , field_type = Model.Prim Model.PrimText
                , field_cardinality = Model.optional
                , field_comment = Model.noComment
                , field_meta = CdmField "id" Nothing
                }
            , Model.Field
                { field_name = "globalKey"
                , field_type = Model.Prim Model.PrimText
                , field_cardinality = Model.optional
                , field_comment = Model.noComment
                , field_meta = CdmField "globalKey" Nothing
                }
            , Model.Field
                { field_name = "externalKey"
                , field_type = Model.Prim Model.PrimText
                , field_cardinality = Model.optional
                , field_comment = Model.noComment
                , field_meta = CdmField "externalKey" Nothing
                }
            ]
            Model.noComment
        , Model.RecordType
            "ZonedDateTime"
            [ Model.Field
                { field_name = "dateTime"
                , field_type = Model.Prim Model.PrimTime
                , field_cardinality = Model.single
                , field_comment = Model.noComment
                , field_meta = CdmField "dateTime" Nothing
                }
            , Model.Field
                { field_name = "timezone"
                , field_type = Model.Prim Model.PrimText
                , field_cardinality = Model.single
                , field_comment = Model.noComment
                , field_meta = CdmField "timezone" Nothing
                }
            ]
            Model.noComment
        ]
      , module_comment = Model.noComment
      }
