import json
import os
from dictdiffer import diff
import requests

from message_integration.metadata.cdm.cdmMetaDataReader import CdmMetaDataReader
from message_integration.metadata.damlTypes import DamlType, Record
from message_integration.strategies.jsonCdmDecodeStrategy import JsonCdmDecodeStrategy
from message_integration.strategies.jsonCdmEncodeStrategy import JsonCdmEncodeStrategy

if __name__ == '__main__':
  with open('../metadata/CDM.json') as metadataRaw:

    metadata = CdmMetaDataReader().fromJSON(json.load(metadataRaw))

    urlCreate = "http://localhost:7575/command/create"
    urlSearch = "http://localhost:7575/contracts/search"
    headers = {"Content-Type": "application/json", "Authorization": "Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJsZWRnZXJJZCI6Ik15VGVzdExlZGdlciIsImFwcGxpY2F0aW9uSWQiOiJDZG1UZXN0IiwicGFydHkiOiJUZXN0ZXIifQ.tHhViFI_0P3aLUeij9Es3eE1muNFFtkF8HnkFeQK91E"}

    for filename in os.listdir('../examples'):
      if filename.endswith("json"):
        with open('../examples/' + filename) as inJson:
          # Decode json to match http-json-api
          inJsonDict = json.load(inJson)
          e = JsonCdmDecodeStrategy(metadata).decode(inJsonDict, Record("Event"))

          # Create contract and get resulting contract id
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

            # Get contract via contract id
            arg = { "%templates": [{
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
