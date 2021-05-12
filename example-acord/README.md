# FpML Clearing Demo

## Overview

This example shows a simple FpML-based workflow orchestrated via Daml. It shows how messages are ingested, how to work with FpML data structures from Daml, and how to output FpML messages from the ledger. This example is compatible with version 5.10 of the FpML specification.

## Workflow
1. The operator ingests a `RequestClearing` FpML message via the `SubmitRequestClearing` choice on the `ClearingHouseRole` contract.
2. Party information is extracted from the message in Daml and `RequestAcknowledgementEvent` and `RequestClearingEvent` contracts are created.
3. The application exercises the `DispatchRequestClearing`  choice on `RequestClearingEvent` providing contract ids of clearning member roles.
4. The `HandleRequestClearing` choice is exercised on both `ClearingMemberRole` contracts, which generates the final `ClearingConfirmedEvent` contracts.

## Prerequisites

- [Install the Daml SDK](https://docs.daml.com/getting-started/installation.html)
  - Make sure to [configure the Maven repository](https://docs.daml.com/getting-started/installation.html#configure-maven)
- [Java Integration Libarary](../lib-integration-java/README.md)
  - Make sure to install the library locally using `mvn install -DskipTests`
- [Scala SBT](https://www.scala-sbt.org/)

## Building

To build the example:
```
daml build
```
To generate Markdown documentation for the FpML Daml sources:
```
daml damlc docs -o target -f Html daml/FpML/V510/Confirmation.daml
```
You can the open `target/FpML-V510-Confirmation.md` to browse the documentation.

## Running
From the example root directory:
```
daml start
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
