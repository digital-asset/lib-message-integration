# CDM example

This is a small example to illustrate how to load and retrieve CDM messages to and from the ledger. Currently only the [Python Integration Library](../../lib-integration-python/README.md) is used.

## Setup

1. Build the DAML model by running

        daml build

2. Build the python application by running

        (cd integration-python; pipenv install libs/message_integration-0.0.1-py3-none-any.whl)

## How to start

Start each of the following commands in a separate shell.

1. Start the sandbox via

        daml sandbox .daml/dist/cdmIntro-0.0.1.dar -w --ledgerid "MyTestLedger"

2. Start the Http-Json-Api via

        daml json-api --ledger-host localhost --ledger-port 6865 --http-port 7575 --max-inbound-message-size 52428800

## How to use

Execute the python app via

      (cd integration-python; pipenv run python3 main.py)

For each [example file](examples), the app performs the following steps:
1. Decode the example json to match the Http-Json-Api representation.
2. Create a contract on the ledger via the Http-Json-Api.
3. Retrieve the contract from the ledger via the Http-Json-Api.
4. Encode the contract to get a CDM json representation
5. Compare if the input and output jsons are identical.
