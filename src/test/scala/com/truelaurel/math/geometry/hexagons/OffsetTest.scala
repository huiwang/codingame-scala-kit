package com.truelaurel.math.geometry.hexagons

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by hwang on 20/04/2017.
  */
class OffsetTest extends FlatSpec with Matchers{

  behavior of "OffsetTest"

  it should "compute angle" in {
    Offset(4, 6).angle(Offset(4, 6)) should be(0)
    Offset(4, 6).angle(Offset(5, 6)) should be(0)
    Offset(4, 6).angle(Offset(4, 5)) should be(1)
    Offset(4, 6).angle(Offset(3, 6)) should be(3)
    Offset(4, 6).angle(Offset(4, 7)) should be(5)
  }

}
