# import dazl
# from dazl.model.reading import ReadyEvent
import json
import os
from collections import namedtuple
from lib.metadata.cdm.cdmMetaDataReader import CdmMetaDataReader
from lib.metadata.damlTypes import DamlType, Record
from lib.strategies.jsonCdmDecodeStrategy import JsonCdmDecodeStrategy
from lib.strategies.jsonCdmEncodeStrategy import JsonCdmEncodeStrategy
import asyncio
from dictdiffer import diff
import requests

if __name__ == '__main__':
  direcotry = '../example/cdm/'

  with open(direcotry + 'metadata/CDM.json') as metadataRaw:

    metadata = CdmMetaDataReader().fromJSON(json.load(metadataRaw))

    urlCreate = "http://localhost:7575/command/create"
    urlSearch = "http://localhost:7575/contracts/search"
    headers = {"Content-Type": "application/json", "Authorization": "Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJsZWRnZXJJZCI6InNhbmRib3gtNzAzZGFlMzYtYmFmZi00ZmE1LWFhZDYtYmJmMzFkMTRlOTdmIiwiYXBwbGljYXRpb25JZCI6IkNkbVRlc3QiLCJwYXJ0eSI6IlRlc3RlciJ9.kImpUYIZauvUag634t8cs8oiH0GVFFKBlZdH0WMhGhg"}

    for filename in os.listdir(direcotry + 'examples'):
      if filename.endswith("json"):
        with open(direcotry + 'examples/' + filename) as inJson:
          # Decode json to match http-json-api
          inJsonDict = json.load(inJson)
          e = JsonCdmDecodeStrategy(metadata).decode(inJsonDict, Record("Event"))

          # Create Contract and get resulting contract id
          arg = { "templateId": {
                    "moduleName": "Main",
                    "entityName": "EventInstance"
                  },
                  "argument": {
                      "e": e,
                      "ps": ["Tester"]
                  }
                }
          res = requests.post(urlCreate, json = arg, headers = headers)
          resJson = json.loads(res.text)
          if resJson["status"] != 200:
            print(resJson)
          else:
            contractId = resJson["result"]["contractId"]

            # Get event from created contract
            arg = { "templateIds": [{
                        "moduleName": "Main",
                        "entityName": "EventInstance"
                      }]
                  }
            res = requests.post(urlSearch, json = arg, headers = headers)
            resJson = json.loads(res.text)
            if resJson["status"] != 200:
              print(resJson)
            else:
              e = {}
              for r in resJson["result"]:
                for ac in r["activeContracts"]:
                  if ac["contractId"] == contractId:
                    e = r["activeContracts"][0]["argument"]["e"]

              # Encode
              outJsonDict = JsonCdmEncodeStrategy(metadata).encode(e, Record("Event"))

              # Compare
              difference = list(diff(inJsonDict, outJsonDict))
              print("Difference for " + filename + ": " + str(difference))
