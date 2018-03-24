name := "CodinGame-Scala-Kit"
version := "0.1.0"
scalaVersion := "2.12.4"

resolvers += Resolver.mavenLocal

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
libraryDependencies += "com.github.writethemfirst" % "approvals-java" % "0.4-SNAPSHOT" % "test"
libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.12.4" % "test"
libraryDependencies += "com.geirsson" %% "scalafmt-core" % "1.3.0"
libraryDependencies += "com.geirsson" %% "scalafmt-cli" % "1.3.0"

enablePlugins(JmhPlugin)
enablePlugins(JavaAppPackaging)