from dictdiffer import diff
import json
import os
import unittest

from ..metadata.cdm.cdmMetaDataReader import CdmMetaDataReader
from ..metadata.damlTypes import DamlType, Record
from ..strategies.jsonCdmDecodeStrategy import JsonCdmDecodeStrategy
from ..strategies.jsonCdmEncodeStrategy import JsonCdmEncodeStrategy

class Test(unittest.TestCase):

  def __init__(self, metaData, directory: str, fileName: str):
    super(Test, self).__init__('runTest')
    self.metaData = metaData
    self.directory = directory
    self.fileName = fileName

  def runTest(self):
    with open(self.directory + 'examples/' + self.fileName) as inJson:
      inJsonDict = json.load(inJson)
      decoded = JsonCdmDecodeStrategy(self.metaData).decode(inJsonDict, Record("Event"))
      outJsonDict = JsonCdmEncodeStrategy(self.metaData).encode(decoded, Record("Event"))

      difference = list(diff(inJsonDict, outJsonDict))
      self.assertEqual(difference, [], msg = "Test " + self.fileName.replace(".json", "") + " failed")


def runTests(directory):
  suite = unittest.TestSuite()

  with open(directory + 'metadata/CDM.json') as metadataRaw:
    metaData = CdmMetaDataReader().fromJSON(json.load(metadataRaw))

    for fileName in os.listdir(directory + 'examples'):
      if fileName.endswith("json"):
        suite.addTest(Test(metaData, directory, fileName))

  unittest.TextTestRunner().run(suite)
