name := "CodinGame-Scala-Kit"
version := "0.1.0"
scalaVersion := "2.12.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

libraryDependencies += "org.mockito"  % "mockito-core" % "2.8.9" % "test"

enablePlugins(JmhPlugin)