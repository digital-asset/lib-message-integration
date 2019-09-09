from dataclasses import dataclass

@dataclass(frozen=True)
class CdmFieldMeta():
  ""

@dataclass(frozen=True)
class CdmEnum(CdmFieldMeta):
  name: str

@dataclass(frozen=True)
class CdmField(CdmFieldMeta):
  name: str
