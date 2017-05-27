package com.truelaurel.tests

import java.io.File

import scala.reflect.internal.util.BatchSourceFile
import scala.tools.nsc.{GenericRunnerSettings, Global}

object Compilation {
  val settings = new GenericRunnerSettings(System.out.println _)
  val sbtClasspath = System.getProperty("sbt-classpath")
  val s = File.pathSeparator
  val classPath = s".${s}$sbtClasspath"
  settings.classpath.append(classPath)
  settings.usejavacp.value = true
  val global = new Global(settings)

  def compiles(code: String): Boolean = {
    val r = new global.Run
    r.compileSources(List(new BatchSourceFile("<partest>", code)))
    val errors = global.reporter.hasErrors
    if (errors) r.reporting.summarizeErrors()
    !errors
  }
}
