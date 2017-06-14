package com.truelaurel.codingame.time

import scala.concurrent.duration.Duration
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by Wei on 14/06/2017.
  */
class ChronometerTest extends FlatSpec with Matchers {

  behavior of "Chronometer"

  val millis = 1000
  val duration = Duration(millis, "millis")
  val cs = new Chronometer(duration)
  cs.start()

  it should "return false while counter still alive" in {
    cs.willOutOfTime should equal(false)
  }

  it should "return true after counter ran out" in {
    Thread.sleep(millis)
    cs.willOutOfTime should equal(true)
  }
}
