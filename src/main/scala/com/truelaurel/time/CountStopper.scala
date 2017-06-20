package com.truelaurel.time

/**
  * Created by hwang on 20/04/2017.
  */
class CountStopper(count: Int) extends Stopper {
  private var remaining = count

  override def start(): Unit = {}

  override def willOutOfTime: Boolean = {
    remaining = remaining - 1
    remaining < 0
  }
}
