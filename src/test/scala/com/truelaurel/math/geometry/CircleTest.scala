package com.truelaurel.math.geometry

import org.scalatest.{FlatSpec, Matchers}

class CircleTest extends FlatSpec with Matchers {

  "intersection" should "be empty for circles too far away" in {
    Circle(Pos(100, 150), 100)
      .intersections(Circle(Pos(500, 150), 100)) shouldBe empty
  }

  it should "be a single point when tangent" in {
    Circle(Pos(0, 0), 100).intersections(Circle(Pos(200, 0), 100)) shouldBe Seq(
      Pos(100, 0)
    )
    Circle(Pos(0, 400), 200)
      .intersections(Circle(Pos(600, 400), 400)) shouldBe Seq(Pos(200, 400))
  }

  it should "be a 2 points when secant" in {
    Circle(Pos(0, 0), 100).intersections(Circle(Pos(0, 75), 100)) should
      contain theSameElementsAs Seq(Pos(92, 37), Pos(-92, 37))

    Circle(Pos(0, 50), 100).intersections(Circle(Pos(50, 0), 100)) should
      contain theSameElementsAs Seq(Pos(91, 91), Pos(-41, -41))
    //detects rounding bug
    Circle(Pos(1735, 542), 180)
      .intersections(Circle(Pos(1476, 490), 220)) should
      contain theSameElementsAs List(Pos(1664, 376), Pos(1605, 667))
  }
}
