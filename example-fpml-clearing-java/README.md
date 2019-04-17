# FpML Clearing Demo

## Overview

This example shows a simple FpML-based workflow orchestrated via DAML. It shows how messages are ingested, how to work with FpML data structures from DAML, and how to output FpML messages from the ledger. This example is compatible with version 5.10 of the FpML specification.

## Workflow
1. The operator ingests an `RequestClearing` FpML message via the `SubmitRequestClearing` choice on the `ClearingHouseRole` contract.
2. Party information is extracted from the message in DAML and `RequestAcknowledgementEvent` and `RequestClearingEvent` contracts are created.
3. The application exercises the `DispatchRequestClearing`  choice on `RequestClearingEvent` providing contract ids of clearning member roles.
4. The `HandleRequestClearing` choice is exercises on both `ClearingMemberRole` contracts, which generates the final `ClearingConfirmedEvent` contracts.

## Prerequisites

- [DAML SDK](https://docs.daml.com/getting-started/installation.html)
- [Java Integration Libarary](../lib-integration-java/README.md)
- [Scala SBT](https://www.scala-sbt.org/)

## Building

```
da compile
```

## Running
From the example root directory:
```
da start sandbox
cd app
sbt "runMain com.digitalasset.app.Bots"
```
Then, in a separate console:
```
cd app
sbt "runMain com.digitalasset.app.REPL"
```
In the REPL:
```
send(requestClearing)
```
This will send the `RequestClearing` message to the ledger, and display the resulting `ClearingConfirmed` messages on the console.