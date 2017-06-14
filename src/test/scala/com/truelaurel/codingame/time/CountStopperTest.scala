package com.truelaurel.codingame.time

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by Wei on 14/06/2017.
  */
class CountStopperTest extends FlatSpec with Matchers {

  behavior of "CountStopper"

  val stopcount: Int = 200
  val cs = new CountStopper(stopcount)
  cs.start()

  it should "return false while counter still alive" in {
    for (i <- 0 until stopcount)
      cs.willOutOfTime should equal(false)
  }

  it should "return true after counter ran out" in {
      cs.willOutOfTime should equal(true)
  }
}
