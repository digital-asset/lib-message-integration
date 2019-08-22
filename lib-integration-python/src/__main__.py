import dazl
from dazl.model.reading import ReadyEvent
import json
import os
from collections import namedtuple
from lib.metadata.cdm.cdmMetaDataReader import CdmMetaDataReader
from lib.metadata.damlTypes import DamlType, Record
from lib.strategies.jsonCdmDecodeStrategy import JsonCdmDecodeStrategy
import asyncio


# def register_archive_expired_process(network: dazl.Network):
#   client = network.aio_party('Tester')

#   @client.ledger_ready()
#   async def init(event: ReadyEvent):
#     cs = event.acs_find_active('Main.ContractInstance')
#     print(cs)

if __name__ == '__main__':
  url = 'http://localhost:6865'
  network = dazl.Network()
  network.set_config(url=url)
  # register_archive_expired_process(network)

  direcotry = '../example/cdm_v2_0_136/'

  with open(direcotry + 'metadata/CDM.json') as metadataRaw:
    url = 'http://localhost:6865'
    network = dazl.Network()
    network.set_config(url=url)
    client = network.aio_party('Tester')

    metadata = CdmMetaDataReader().fromJSON(json.load(metadataRaw))

    with dazl.simple_client(url, 'Tester') as client:
      for filename in os.listdir(direcotry + 'examples'):
        with open(direcotry + 'examples/' + filename) as exJson:

          d = JsonCdmDecodeStrategy(metadata).decode(json.load(exJson), Record("Contract"))
          arg = { 'd': d, 'ps': ['Tester'] }

          client.submit_create('Main.ContractInstance',arg)


  network.run_until_complete()
