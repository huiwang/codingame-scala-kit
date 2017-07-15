name := "CodinGame-Scala-Kit"
version := "0.1.0"
scalaVersion := "2.12.2"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
libraryDependencies += "com.github.alexarchambault" %% "scalacheck-shapeless_1.13" % "1.1.5" % "test"

libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.12.2" % "test"

enablePlugins(JmhPlugin)
enablePlugins(JavaAppPackaging)