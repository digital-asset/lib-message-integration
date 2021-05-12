name := "app"

version := "0.1"

scalaVersion := "2.12.8"

resolvers += Resolver.mavenLocal

libraryDependencies ++= Seq(
  "io.spray" %% "spray-json" % "1.3.4",
  "org.scala-lang" % "scala-compiler" % scalaVersion.value,
  "com.daml" % "bindings-java" % "1.12.0",
  "com.daml" % "bindings-rxjava" % "1.12.0",
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "com.typesafe" % "config" % "1.2.1",
  "com.digitalasset" % "finance-integration" % "0.1-SNAPSHOT",
  "com.google.guava" % "guava" % "30.1.1-jre"
)
