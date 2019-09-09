from dataclasses import dataclass
from enum import Enum
from typing import Generic, TypeVar, List
from .fieldMetaData import FieldMetaData

class Kind(Enum):
  record = 0
  variant = 1

SrcMeta = TypeVar('SrcMeta')

@dataclass(frozen=True)
class TypeDeclarationMetadata(Generic[SrcMeta]):
  name: str
  kind: Kind
  fields: List[FieldMetaData[SrcMeta]]

def record(name: str, fields):
  return TypeDeclarationMetadata(name, Kind.record, fields)

def variant(name: str, fields):
  return TypeDeclarationMetadata(name, Kind.variant, fields)
