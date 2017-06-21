package com.truelaurel.compilation

import scala.reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox

object Compilation {

  def compiles(code: String): Boolean = {
    val toolBox = currentMirror.mkToolBox()
    toolBox.parse(code)
    true
  }
}
