package com.truelaurel.codingame.time

import scala.concurrent.duration.Duration

/**
  * Created by hwang on 12/02/2017.
  */
class Chronometer(duration: Duration) {
  val timeUnitInNano: Long = duration.toNanos
  private var startTime: Long = 0

  def start(): Unit = startTime = System.nanoTime()
  def isRunOut: Boolean = (System.nanoTime() - startTime) > timeUnitInNano

}
