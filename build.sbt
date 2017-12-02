name := "CodinGame-Scala-Kit"
version := "0.1.0"
scalaVersion := "2.12.4"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.12.4" % "test"

enablePlugins(JmhPlugin)
enablePlugins(JavaAppPackaging)