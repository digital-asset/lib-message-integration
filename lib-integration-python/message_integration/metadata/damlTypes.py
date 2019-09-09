from dataclasses import dataclass
from enum import Enum

class Primitive(Enum):
  TEXT = 0
  BOOL = 1
  INT64 = 2
  DECIMAL = 3
  TIME = 4
  DATE = 5
  UNIT = 6
  PARTY = 7

@dataclass(frozen=True)
class DamlType():
  ""

@dataclass(frozen=True)
class Prim(DamlType):
  prim: Primitive

@dataclass(frozen=True)
class Record(DamlType):
  name: str

@dataclass(frozen=True)
class Variant(DamlType):
  name: str
