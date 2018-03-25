name := "CodinGame-Scala-Kit"
version := "0.1.0"
scalaVersion := "2.12.4"

// solve a weird issue with java dependencies (trait Approving)
// see https://stackoverflow.com/questions/43751394/package-cats-contains-object-and-package-with-same-name-implicits &
// https://github.com/druid-io/tranquility/blob/master/build.sbt
scalacOptions := Seq("-Yresolve-term-conflict:object")


resolvers += Resolver.mavenLocal

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
libraryDependencies += "com.github.writethemfirst" % "approvals-java" % "0.4-SNAPSHOT" % "test"
libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.12.4" % "test"
libraryDependencies += "com.geirsson" %% "scalafmt-core" % "1.3.0"
libraryDependencies += "com.geirsson" %% "scalafmt-cli" % "1.3.0"

enablePlugins(JmhPlugin)
enablePlugins(JavaAppPackaging)
