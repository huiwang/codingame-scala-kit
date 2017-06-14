package com.truelaurel.codingame.dichotomy

import com.truelaurel.codingame.dichotomy.Dichotomy._
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by Wei on 14/06/2017.
  */
class DichotomyTest extends FlatSpec with Matchers {

  behavior of "Dichotomy Search"

  it should "return the cloest value of the criteria" in {
    // if criteria returns true,
    // it means that the current value is still higher than expection
    // thus we should guess a lower value
    search(3, 9, _ > 5) should equal(6)
    search(3, 9, _ >= 5) should equal(5)
  }
}
