name := "example-accord-app"

version := "0.1"

// See: https://docs.scala-lang.org/overviews/jdk-compatibility/overview.html
scalaVersion := "2.12.14"

resolvers += Resolver.mavenLocal

libraryDependencies += "io.spray" %% "spray-json" % "1.3.4"
libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value
libraryDependencies += "com.daml" % "bindings-java" % "1.12.0"
libraryDependencies += "com.daml" % "bindings-rxjava" % "1.12.0"
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3"
libraryDependencies += "com.typesafe" % "config" % "1.2.1"
libraryDependencies += "com.digitalasset" % "finance-integration" % "0.1-SNAPSHOT"
libraryDependencies += "com.google.guava" % "guava" % "30.1.1-jre"

Compile / sourceGenerators += Def.task {
  val generatedSources = (Compile / sourceManaged).value.toGlob / ** / "*.java"
  fileTreeView.value.list(generatedSources).map(_._1.toFile)
}.taskValue

Compile / run / fork := true
Compile / run / javaOptions += "-Djava.protocol.handler.pkgs=com.digitalasset.integration.protocols"

// This seems to too slow and incremental compilation does not seem to work.
//Compile / sourceGenerators += Def.task {
//  import DamlPlugin._
//
//  val modelSettings = DamlCodegenSettings(
//    dar = baseDirectory.value / ".." / "acord-models" / ".daml" / "dist" / "acord-models-0.0.1.dar",
//    outputDirectory = (Compile / sourceManaged).value
//  )
//  damlCodegen(modelSettings)
//  fileInputs += modelSettings.dar.toGlob
//  val modelSources = modelSettings.outputDirectory.toGlob / ** / "*.java"
//  fileOutputs += modelSources
//  val modelFiles = fileTreeView.value.list(modelSources).map(_._1.toFile)
//
//  val setupSettings = modelSettings.copy(dar = baseDirectory.value / ".." / "ledger-setup" / ".daml" / "dist" / "acord-ledger-setup-0.0.1.dar")
//  damlCodegen(setupSettings)
//  fileInputs += setupSettings.dar.toGlob
//  val setupSources = setupSettings.outputDirectory.toGlob / ** / "*.java"
//  fileOutputs += setupSources
//  val setupFiles = fileTreeView.value.list(setupSources).map(_._1.toFile)
//
//  modelFiles ++ setupFiles
//}.taskValue
