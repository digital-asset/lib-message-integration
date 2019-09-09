from dataclasses import dataclass
from typing import Generic, TypeVar, Dict, List
from .typeDeclarationMetadata import TypeDeclarationMetadata
from .damlTypes import DamlType, Record, Variant
from .fieldMetaData import FieldMetaData

A = TypeVar('A')

@dataclass(frozen=True)
class MetaData(Generic[A]):
  schemaName: str
  typeDeclarations: Dict[str, TypeDeclarationMetadata[A]]

  def isEnumValue(self, type: DamlType) -> bool:
    return True # TODO

  def getFields(self, type: DamlType) -> List[FieldMetaData[A]]:
    if isinstance(type, Record) or isinstance(type, Variant) :
      if type.name in self.typeDeclarations:
        return self.typeDeclarations[type.name].fields
      else:
        raise Exception("Unknown type " + str(type))

    return []

from typing import Iterable
