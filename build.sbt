// val dottyVersion = "0.22.0-RC1"
val dottyVersion = "0.23.0-bin-20200213-26de50a-NIGHTLY"

lazy val root = project
  .in(file("."))
  .settings(
    name := "dotty-simple",
    version := "0.1.0",

    scalaVersion := dottyVersion,

    libraryDependencies += "com.google.protobuf" % "protobuf-java" % "3.11.0"
  )
