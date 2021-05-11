# Daml Message Integration Library

## Overview

The Daml Integration Library is a set of projects to facilitate the integration of Daml-based ledgers with external systems. It contains tools, libraries, and examples that demonstrate how to interact with a ledger via messaging standards.

## Metagen Tool
This tool is used to generate Daml types and type metadata from message format specifications. It currently supports type generation from XSD schemas and is used by the Java Integration Library to implement enocder and decoders for XML messages.

[Project Readme](tool-metagen/README.md)

## Java Integration Library
This library is used to decode messages to Ledger API values and to encode such values back into messages. It contains an example implementation for the ISDA FpML message standard.

[Project Readme](lib-integration-java/README.md)

## FpML Clearing Example
This example shows how to integrate a Daml model with an FpML message workflow and how to extend the workflow within Daml. It demonstrates a simple clearing process, which is conformant with the FpML standard.

[Project Readme](example-fpml-clearing-java/README.md)
