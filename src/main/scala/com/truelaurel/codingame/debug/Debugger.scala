package com.truelaurel.codingame.debug

/**
  * Created by hwang on 29/04/2017.
  */
object Debugger {

  val enabled = true

  def debug(message: Any): Unit = {
    if (enabled) System.err.println(message)
  }
}
