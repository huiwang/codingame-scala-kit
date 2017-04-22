package com.truelaurel.codingame.caribbean.common

import com.truelaurel.codingame.hexagons.Offset
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by hwang on 22/04/2017.
  */
class CollisionAnalysisTest extends FlatSpec with Matchers {

  behavior of "CollisionAnalysisTest"

  it should "collisionTime" in {
    val ship = Ship(0, Offset(7, 10), 0, 1, 10, 1)
    CollisionAnalysis.collisionTime(ship, Offset(7, 9).toCube) should be(2)
    CollisionAnalysis.collisionTime(ship, Offset(9, 10).toCube) should be(1)
  }

}
