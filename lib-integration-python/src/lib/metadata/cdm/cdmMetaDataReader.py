from ..metaDataReader import MetaDataReader
from .cdmFieldMeta import CdmField, CdmEnum

class CdmMetaDataReader(MetaDataReader):
  def parseFieldMeta(self, dict):
    src = dict['src']
    if src == "field":
      return CdmField(dict['name'])
    if src == "enum":
      return CdmEnum(dict['name'])
    raise Exception("unknown src " + src)
