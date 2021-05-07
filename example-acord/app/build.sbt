import scala.util.Try

name := "app"

version := "0.1"

scalaVersion := "2.12.8"

libraryDependencies += "io.spray" %%  "spray-json" % "1.3.4"
libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value
libraryDependencies += "com.daml" % "bindings-java" % "1.11.0"
libraryDependencies += "com.daml" % "bindings-rxjava" % "1.11.0"
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3"
libraryDependencies += "com.typesafe" % "config" % "1.2.1"
libraryDependencies += "com.digitalasset" % "finance-integration" % "0.1-SNAPSHOT"

externalResolvers ++= Seq(
  Resolver.mavenLocal,
)
