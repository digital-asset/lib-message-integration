from dataclasses import dataclass
from enum import Enum
from typing import Generic, TypeVar
from .damlTypes import DamlType

SrcMeta = TypeVar('SrcMeta')

class Cardinality(Enum):
  OPTIONAL = 0
  SINGLE = 1
  MANY = 2
  MANY1 = 3

@dataclass(frozen=True)
class FieldMetaData(Generic[SrcMeta]):
  name: str
  type: DamlType
  cardinality: Cardinality
  meta: SrcMeta

