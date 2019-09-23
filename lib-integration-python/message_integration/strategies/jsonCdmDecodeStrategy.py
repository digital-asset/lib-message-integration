from dataclasses import dataclass
import re
from ..metadata.metaData import MetaData
from ..metadata.cdm.cdmFieldMeta import CdmFieldMeta, CdmField
from ..metadata.fieldMetaData import FieldMetaData, Cardinality
from ..metadata.damlTypes import *
from dateutil.parser import parse
from datetime import timezone, date

@dataclass(frozen=True)
class JsonCdmDecodeStrategy():
  metadata: MetaData[CdmFieldMeta]

  def decode(self, elem, type: DamlType):
   return self.decodeRecord(elem, type, "")

  def decodeRecord(self, elem, type: DamlType, path: str):
    rec = {}
    childFields = self.metadata.getFields(type)
    for childField in childFields:
      decoded = self.decodeField(elem, childField, path)
      if not decoded is None:
        rec[childField.name] = decoded
    return rec

  def decodeField(self, parentElem, field: FieldMetaData[CdmFieldMeta], parentPath: str):
    try:
      if isinstance(field.meta, CdmField):
        elemName = field.meta.name
        path = parentPath + "/" + elemName

        fieldValue = parentElem[elemName] if elemName in parentElem else None
        return self.applyCardinality(fieldValue, field.type, field.cardinality, path)

      else:
        raise Exception("Unexpected metadata for field " + str(field) + "at path " + path)
    except Exception as e:
      raise Exception("Error at path " + path + ": " + str(e))

  def applyCardinality(self, elem, type: DamlType, card: Cardinality, path: str):
    if card == Cardinality.MANY or card == Cardinality.MANY1:
      values = []
      if elem is not None:
        for e in elem:
          decoded = self.decodeElement(e, type, path)
          values.append(decoded)

      if card ==  Cardinality.MANY1 and len(values) == 0:
        raise Exception("Unexpected empty array at " + path)

      return values

    elif card == Cardinality.OPTIONAL:
      res = {}
      if elem is not None:
        res = self.decodeElement(elem, type, path)
      else:
        res = None
      return res

    elif card == Cardinality.SINGLE:
      if elem is not None:
        return self.decodeElement(elem, type, path)
      else:
        raise Exception("Missing value at " + path)

    else:
      raise Exception("Unknown cardinality " + str(card))

  def decodeElement(self, elem, type: DamlType, path: str):
    if isinstance(type, Record) and type.name == "ZonedDateTime":
      dt = parse(elem)
      return {"dateTime": dt.astimezone(timezone.utc).replace(tzinfo=None).isoformat() + 'Z', "timezone": "UTC"}

    elif isinstance(type, Record):
      return self.decodeRecord(elem, type, path)

    elif isinstance(type, Variant):
      if self.metadata.isEnumValue(type):
        return self.decodeEnum(elem, type, path)
      else:
        raise Exception("Unexpected variant at " + path)

    elif isinstance(type, Prim):
      return self.decodePrim(elem, type, path)

    else:
      raise Exception("Unsupported type " + str(type) + " at path " + path)

  def decodeEnum(self, elem, type: DamlType, path: str):
    childFields = self.metadata.getFields(type)
    for childField in childFields:
      enumName = childField.meta.name
      if elem == enumName or elem == re.sub( '(?<!^)(?=[A-Z])', '_', enumName ).upper():
        return childField.name

    raise Exception(" Unable to match enum value of " + elem + " with type " + str(type) + " at " + path)

  def decodePrim(self, elem, prim: Prim, path: str):
    if prim.prim == Primitive.DATE:
      return date(elem['year'], elem['month'], elem['day']).isoformat()
    else:
      return elem
