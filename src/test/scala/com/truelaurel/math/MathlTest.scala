package com.truelaurel.math

import com.truelaurel.math.Mathl._
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by Wei on 14/06/2017.
  */
class MathlTest extends FlatSpec with Matchers {

  behavior of "Random Number"

  it can "change" in {
    for (i <- 0 until 100)
      random.nextInt should not equal(random.nextInt)
  }

  it can "set a range" in {
    for (i <- 0 until 100) {
      val value = randomBetween(-1.0, 1.0)
      value should be <= 1.0
      value should be >= -1.0
    }
  }

  behavior of "Half Up"

  it should "have different behavior for positive and negative number" in {
    val value1 = 1.5
    val value2 = -1.5
    halfUp(value1).abs should not equal(halfUp(value2))
  }

  behavior of "Almost Equal"

  it should "have no difference dealing with computing error" in {
    val value1 = 0.0
    val value2 = 0.000001
    for (i <- 0 until 100)
      almostEqual(randomBetween(value1, value2), randomBetween(value1, value2)) should equal(true)
  }
}
