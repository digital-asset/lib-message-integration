# ACORD Demo

Demonstrate the ingestion of an [ACORD](https://www.acord.org/standards-architecture/acord-data-standards) message.
## Prerequisites

- [Install the Daml SDK](https://docs.daml.com/getting-started/installation.html)
    - Make sure to [configure the Maven repository](https://docs.daml.com/getting-started/installation.html#configure-maven)
- [Java Integration Library](../lib-integration-java/README.md)
    - Make sure to install the library locally using `mvn install -DskipTests`
- [Scala SBT](https://www.scala-sbt.org/)

## Running the Example

To build the example:

```shell
make
```

Start a sandbox ledger:

```shell
make start
```

To run the demo app, pass it a suitable ACORD XML message:

```shell
cd app
sbt "runMain PoCApp [URL-to-XML]"
```
