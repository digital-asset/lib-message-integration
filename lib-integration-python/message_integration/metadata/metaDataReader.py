from abc import ABC, abstractmethod
from .metaData import MetaData
from .typeDeclarationMetadata import record, variant
from .fieldMetaData import FieldMetaData, Cardinality
from .damlTypes import *

class MetaDataReader(ABC):
  def fromJSON(self, dict):
    schemaName = dict['schema']

    typeDeclarations = {}
    for decl in dict['declarations']:
      typeDeclarations[decl['name']] = self.parseTypeDeclarion(decl)

    return MetaData(schemaName, typeDeclarations)

  def parseTypeDeclarion(self, dict):
    name = dict['name']

    fields = []
    for field in dict['fields']:
      fields.append(self.parseField(field))

    if dict['kind'] == "record":
      return record(name, fields)
    if  dict['kind'] == "variant":
      return variant(name, fields)

  def parseField(self, dict):
    name = dict['name']
    card = self.parseCardinality(dict['cardinality'])
    type = self.parseDamlType(dict['type'])
    meta = self.parseFieldMeta(dict['meta'])
    return FieldMetaData(name, type, card, meta)

  def parseDamlType(self, dict):
    name = dict['name']
    kind = dict['kind']
    if (kind == "record"):
      return Record(name)
    if (kind == "variant"):
      return Variant(name)
    if (kind == "prim"):
      if name == "TEXT":
        return Prim(Primitive.TEXT)
      if name == "BOOL":
        return Prim(Primitive.BOOL)
      if name == "INT64":
        return Prim(Primitive.INT64)
      if name == "DECIMAL":
        return Prim(Primitive.DECIMAL)
      if name == "TIME":
        return Prim(Primitive.TIME)
      if name == "DATE":
        return Prim(Primitive.DATE)
      if name == "UNIT":
        return Prim(Primitive.UNIT)
      if name == "PARTY":
        return Prim(Primitive.PARTY)
      raise Exception("unknown primitive " + name)
    raise Exception("unknown kind " + kind)

  def parseCardinality(self, card: str):
    if (card == "MANY"):
      return Cardinality.MANY
    if (card == "MANY1"):
      return Cardinality.MANY1
    if (card == "OPTIONAL"):
      return Cardinality.OPTIONAL
    if (card == "SINGLE"):
      return Cardinality.SINGLE
    raise Exception("unknown cardinality " + card)

  @abstractmethod
  def parseFieldMeta(self, dict):
    pass
