package com.truelaurel.codingame.logging

/**
  * Created by hwang on 29/04/2017.
  */
object CGLogger {

  val info = 0
  val debug = 1

  var current = info

  def debug(message: Any): Unit = log(message, debug)

  def info(message: Any): Unit = log(message, info)

  private def log(message: Any, level: Int): Unit = {
    if (level <= current) {
      System.err.println(message)
    }
  }

}
