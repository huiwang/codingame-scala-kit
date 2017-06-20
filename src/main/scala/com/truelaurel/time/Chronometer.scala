package com.truelaurel.time

import scala.concurrent.duration.Duration

/**
  * Created by hwang on 12/02/2017.
  */
class Chronometer(duration: Duration) extends Stopper {
  val budget: Long = duration.toNanos
  private var startTime: Long = 0
  private var elapsed: Long = 0
  private var maxTurnElapsed: Long = -1

  override def start(): Unit = {
    startTime = mark
    maxTurnElapsed = -1
    elapsed = 0
  }

  def mark: Long = {
    System.nanoTime()
  }

  override def willOutOfTime: Boolean = {
    val untilNow = mark - startTime
    val currentTurnElapsed = untilNow - elapsed
    maxTurnElapsed = Math.max(maxTurnElapsed, currentTurnElapsed)
    val remaining = budget - untilNow
    elapsed = untilNow
    //System.err.println(s"remaining $remaining max $maxTurnElapsed")
    remaining < maxTurnElapsed
  }

}