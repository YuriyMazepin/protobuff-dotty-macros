// val dottyVersion = "0.22.0-RC1"
val dottyVersion = "0.23.0-bin-20200217-e8fa408-NIGHTLY"
val scala213Version = "2.13.1"

lazy val root = project.in(file(".")).settings(
  name := "proto",
  libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
  skip in publish := true,
  scalaVersion := dottyVersion,
).dependsOn(macros).aggregate(macros)

lazy val macros = project
  .in(file("macros"))
  .settings(
    name := "protobutff-dotty-macros",
    version := "0.1.0",
    scalaVersion := dottyVersion,
    crossScalaVersions := Seq(dottyVersion, scala213Version),
    libraryDependencies += "com.google.protobuf" % "protobuf-java" % "3.11.0"
  )
