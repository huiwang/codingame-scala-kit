package com.truelaurel.tests

import scala.reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox

object Compilation {

  def compiles(code: String): Boolean = {
    val toolBox = currentMirror.mkToolBox()
    try {
      toolBox.parse(code)
      true
    } catch {
      case e: Throwable => {
        e.printStackTrace()
        false
      }
    }
  }
}
