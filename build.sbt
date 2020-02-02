val dottyVersion = "0.21.0-RC1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "dotty-simple",
    version := "0.1.0",

    scalaVersion := dottyVersion,

    libraryDependencies += "com.google.protobuf" % "protobuf-java" % "3.11.0",
    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
  )
