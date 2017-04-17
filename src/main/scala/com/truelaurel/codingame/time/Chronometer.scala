package com.truelaurel.codingame.time

import scala.concurrent.duration.Duration

/**
  * Created by hwang on 12/02/2017.
  */
class Chronometer(duration: Duration) {
  val budget: Long = duration.toNanos
  private var startTime: Long = 0
  private var elapsed: Double = 0
  private var count: Long = 0
  private val margin = 2

  def start(): Unit = startTime = System.nanoTime()

  def outOfTime: Boolean = {
    if (count != 0 && budget - elapsed < margin * elapsed / count) {
      true
    } else {
      count += 1
      elapsed = System.nanoTime() - startTime
      elapsed > budget
    }

  }

}
