# The Java Integration Library

## Overview

The Java Integration Library simplifies message integration with DAML-based ledgers. It provides abstractions for encoders and decoders, that convert between message formats and the [Ledger API](https://docs.daml.com/packages/ledger-api-introduction/index.html#ledger-api). An encoding **strategy** takes a Ledger API value and converts it into a message, while a decoding strategy takes a message and converts it into a Ledger API value.

## Strategies

### XSD / XML Strategy
This strategy converts XML messages to and from Ledger API values following an XSD format specification. The `metagen` tool is used to generate DAML types and type metadata for a given XSD schema. This metadata is then used when encoding / decoding corresponding XML messages.

## Examples
An example in the library demonstrates how encoders and decoders can be implemented using the above strategies. A more comprehensive example is available in the [FpML Clearing Demo](http://missinglink).

### FpML Example
The decoder in `ExampleFpmlDecoder.java` demonstrates how to implement a decoder that takes an FpML message and translates it to an exercise command for a choice on a given contract. The example assumes that a contract with a choice that corresponds to the root element name in the FpML message exists, which takes the message as its only argument. If your model is different you can adopt this example accordingly.

The encoder in `ExampleFpmlEncoder.java` takes a `Value` received in a ledger event and translates it into a valid FpML message. It requires a choice name as a constructor argument, which is used (lower-cased) as the root element name of the FpML message. 
These examples are compatible with version 5.10 of the FpML specification.

## Prerequisites

- [Maven](https://maven.apache.org/)

## Building, Packaging, and Installing

To compile the project:
```
mvn compile
```

To build the artefacts:
```
mvn package
```

This will produce the following files:
* finance-integration-{VERSION}-api.jar - integration interfaces only
* finance-integration-{VERSION}.jar - integration library code only
* finance-integration-{VERSION}-fat.jar - fat jar containing all dependencies
* finance-integration-{VERSION}-javadoc.jar - javadoc of interface library code
* finance-integration-{VERSION}-sources.jar - examples sources

To install into a local maven repository:
```
mvn install -DskipTests
```
