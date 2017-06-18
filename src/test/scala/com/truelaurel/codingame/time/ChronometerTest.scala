package com.truelaurel.codingame.time

import java.util.concurrent.TimeUnit

import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration.Duration

/**
  * Created by Wei on 14/06/2017.
  */
class ChronometerTest extends FlatSpec with Matchers {

  behavior of "Chronometer"

  val duration = Duration(1, TimeUnit.SECONDS)

  it should "return false while counter still alive" in {
    val cs = new Chronometer(duration) {

      var predefinedClock = new PredefinedClock(Vector(0, duration.toNanos / 2))

      override def mark: Long = {
        predefinedClock.tick()
      }
    }
    cs.start()
    cs.willOutOfTime should equal(false)
  }

  it should "return true after counter ran out" in {
    val cs = new Chronometer(duration) {

      var predefinedClock = new PredefinedClock(Vector(0, duration.toNanos * 2))

      override def mark: Long = {
        predefinedClock.tick()
      }
    }
    cs.start()
    cs.willOutOfTime should equal(true)
  }
}

class PredefinedClock(values: Vector[Long]) {
  private var i = -1

  def tick(): Long = {
    i += 1
    require(i < values.size, "All predefined values are consumed")
    values(i)
  }
}
