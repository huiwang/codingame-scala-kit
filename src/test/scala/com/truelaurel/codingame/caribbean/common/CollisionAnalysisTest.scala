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

  it should "predict mine" in {
    val my = Ship(1, Offset(11, 10), 0, 2, 10, 1)
    val other = Ship(0, Offset(7, 10), 0, 2, 10, 1)
    CollisionAnalysis.canMine(my, other) should be(true)
  }

}
