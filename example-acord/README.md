# Acord Demo

## Prerequisites

- [Install the Daml SDK](https://docs.daml.com/getting-started/installation.html)
    - Make sure to [configure the Maven repository](https://docs.daml.com/getting-started/installation.html#configure-maven)
- [Java Integration Library](../lib-integration-java/README.md)
    - Make sure to install the library locally using `mvn install -DskipTests`
- [Scala SBT](https://www.scala-sbt.org/)

## Running the Example

To build the example:

```
make
```

Run the codegen:

```shell
make generateSources
```

Start a sandbox ledger:

```shell
make startSandbox
```

To run the demo app:

```shell
cd app
sbt "runMain PoCApp [URL-to-XML]"
```
