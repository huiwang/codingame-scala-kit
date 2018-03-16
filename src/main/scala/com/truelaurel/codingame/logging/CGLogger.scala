package com.truelaurel.codingame.logging

/**
  * Created by hwang on 29/04/2017.
  */
object CGLogger {

  val info = 0
  val debug = 1
  val warn = -1

  var current = info

  var startOfRound: Long = 0

  def time: Long = (System.nanoTime - startOfRound) / 1000000

  def debug(message: Any): Unit = log(message, debug)

  def info(message: Any): Unit = log(message, info)

  private def log(message: Any, level: Int): Unit = {
    if (level <= current) {
      System.err.println(s"$time ms - $message")
    }
  }

}
