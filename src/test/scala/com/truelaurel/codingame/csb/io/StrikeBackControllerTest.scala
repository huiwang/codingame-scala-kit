package com.truelaurel.codingame.csb.io

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by hwang on 29/04/2017.
  */
class StrikeBackControllerTest extends FlatSpec with Matchers {

  behavior of "StrikeBackControllerTest"

  it should "computeGoal" in {
    StrikeBackController.computeGoal(previousGoal = 1, cpCount = 3, currentGoal = 1) should be(1)
    StrikeBackController.computeGoal(previousGoal = 1, cpCount = 3, currentGoal = 2) should be(2)
    StrikeBackController.computeGoal(previousGoal = 2, cpCount = 3, currentGoal = 0) should be(3)
    StrikeBackController.computeGoal(previousGoal = 3, cpCount = 3, currentGoal = 0) should be(3)
  }

}
