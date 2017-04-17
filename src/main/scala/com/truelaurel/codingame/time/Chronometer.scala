package com.truelaurel.codingame.time

import scala.concurrent.duration.Duration

/**
  * Created by hwang on 12/02/2017.
  */
class Chronometer(duration: Duration, maxInvoc: Int = -1) {
  val budget: Long = duration.toNanos
  private var startTime: Long = 0
  private var elapsed: Double = 0
  private var count: Long = 0
  private val margin = 3

  def start(): Unit = startTime = System.nanoTime()

  def outOfTime: Boolean = {
    count += 1
    if (maxInvoc != -1) count >= maxInvoc else {
      if (count != 0 && budget - elapsed < margin * elapsed / count) {
        true
      } else {
        elapsed = System.nanoTime() - startTime
        elapsed > budget
      }
    }
  }

}
