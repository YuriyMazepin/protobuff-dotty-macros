// val dottyVersion = "0.22.0-RC1"
val dottyVersion = "0.23.0-bin-20200217-e8fa408-NIGHTLY"

lazy val root = project
  .in(file("."))
  .settings(
    name := "protobutff-dotty-macros",
    version := "0.1.0",

    scalaVersion := dottyVersion,

    libraryDependencies += "com.google.protobuf" % "protobuf-java" % "3.11.0"
  )
