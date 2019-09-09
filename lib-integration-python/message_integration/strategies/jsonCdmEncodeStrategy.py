from dataclasses import dataclass
import re
from ..metadata.metaData import MetaData
from ..metadata.cdm.cdmFieldMeta import CdmFieldMeta, CdmField
from ..metadata.fieldMetaData import FieldMetaData, Cardinality
from ..metadata.damlTypes import *

@dataclass(frozen=True)
class JsonCdmEncodeStrategy():
  metadata: MetaData[CdmFieldMeta]

  def encode(self, elem, type: DamlType):
   return self.encodeRecord(elem, type, "")

  def encodeRecord(self, elem, type: DamlType, path: str):
    rec = {}
    childFields = self.metadata.getFields(type)
    for childField in childFields:
      encoded = self.encodeField(elem, childField, path)
      if not encoded is None:
        rec[childField.name] = encoded
    return rec

  def encodeField(self, parentElem, field: FieldMetaData[CdmFieldMeta], parentPath: str):
    if isinstance(field.meta, CdmField):
      elemName = field.meta.name
      path = parentPath + "/" + elemName

      fieldValue = parentElem[elemName] if elemName in parentElem else None
      return self.applyCardinality(fieldValue, field.type, field.cardinality, path)

    else:
      raise Exception("Unexpected metadata for field " + str(field) + "at path " + path)

  def applyCardinality(self, elem, type: DamlType, card: Cardinality, path: str):
    if card == Cardinality.MANY or card == Cardinality.MANY1:
      values = []
      if elem is not None:
        for e in elem:
          encoded = self.encodeElement(e, type, path)
          values.append(encoded)

      if card ==  Cardinality.MANY1 and len(values) == 0:
        raise Exception("Unexpected empty array at " + path)

      if len(values) == 0:
        return None
      else:
        return values

    elif card == Cardinality.OPTIONAL:
      res = {}
      if elem is not None:
        res = self.encodeElement(elem, type, path)
      else:
        res = None
      return res

    elif card == Cardinality.SINGLE:
      if elem is not None:
        return self.encodeElement(elem, type, path)
      else:
        raise Exception("Missing value at " + path)

    else:
      raise Exception("Unknown cardinality " + str(card))

  def encodeElement(self, elem, type: DamlType, path: str):
    if isinstance(type, Record) and type.name == "ZonedDateTime":
      return elem['dateTime']

    elif isinstance(type, Record):
      return self.encodeRecord(elem, type, path)

    elif isinstance(type, Variant):
      if self.metadata.isEnumValue(type):
        return self.encodeEnum(elem, type, path)
      else:
        raise Exception("Unexpected variant at " + path)

    elif isinstance(type, Prim):
      return self.encodePrim(elem, type, path)

    else:
      raise Exception("Unsupported type " + str(type) + " at path " + path)

  def encodeEnum(self, elem, type: DamlType, path: str):
    childFields = self.metadata.getFields(type)
    for childField in childFields:
      enumName = childField.name
      if elem == enumName:
        if not childField.meta.name.isupper() and re.match("^[a-zA-Z0-9_]*$", childField.meta.name):
          return re.sub( '(?<!^)(?=[A-Z])', '_', childField.meta.name ).upper()
        else:
          return childField.meta.name

    raise Exception(" Unable to match enum value of " + elem + " with type " + str(type) + " at " + path)

  def encodePrim(self, elem, prim: Prim, path: str):
    if prim.prim == Primitive.INT64:
      return int(elem)
    elif prim.prim == Primitive.DECIMAL:
      return float(elem)
    else:
      return elem
