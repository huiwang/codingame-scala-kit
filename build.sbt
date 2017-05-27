name := "CodinGame-Scala-Kit"
version := "0.1.0"
scalaVersion := "2.12.2"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.12.2"

enablePlugins(JmhPlugin)

val sbtcp = taskKey[Unit]("sbt-classpath")

sbtcp := {
  val files: Seq[File] = (fullClasspath in Compile).value.files
  val sbtClasspath: String = files.map(x => x.getAbsolutePath).mkString(java.io.File.pathSeparator)
  println("Set SBT classpath to 'sbt-classpath' environment variable")
  println(sbtClasspath)
  System.setProperty("sbt-classpath", sbtClasspath)
}

compile <<= (compile in Compile).dependsOn(sbtcp)