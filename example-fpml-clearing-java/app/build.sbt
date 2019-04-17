import scala.util.Try

name := "app"

version := "0.1"

scalaVersion := "2.12.8"

libraryDependencies += "io.spray" %%  "spray-json" % "1.3.4"
libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value
libraryDependencies += "com.daml.ledger" % "bindings-java" % "100.12.1"
libraryDependencies += "com.daml.ledger" % "bindings-rxjava" % "100.12.1"
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3"
libraryDependencies += "com.typesafe" % "config" % "1.2.1"
libraryDependencies += "com.digitalasset" % "finance-integration" % "0.1-SNAPSHOT"

externalResolvers ++= Seq(
  Resolver.mavenLocal,
  "da repository" at "https://digitalassetsdk.bintray.com/DigitalAssetSDK"
)

credentials += Credentials(
  Try(file(System.getenv("SBT_CREDENTIALS"))).getOrElse(Path.userHome / ".sbt" / ".credentials"))
