package com.truelaurel.math.vectorial

import org.scalactic.TolerantNumerics
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by hwang on 04/04/2017.
  */
class VectorlTest extends FlatSpec with Matchers {

  behavior of "Vectorl"
  implicit val doubleEquality = TolerantNumerics.tolerantDoubleEquality(0.01)


  val vectorl = Vectorl(1, 1)
  val sqrtOf2: Double = Math.sqrt(2)

  it should "rotate in degrees" in {
    vectorl.rotateInDegree(90) should be(Vectorl(-1, 1))
    vectorl.rotateInDegree(45) should be(Vectorl(0, sqrtOf2))
    vectorl.rotateInDegree(180) should be(Vectorl(-1, -1))
    vectorl.rotateInDegree(270) should be(Vectorl(1, -1))
    vectorl.rotateInDegree(-45) should be(Vectorl(sqrtOf2, 0))
  }

  it should "rotate in radians" in {
    vectorl.rotateInRadian(Math.PI / 2) should be(Vectorl(-1, 1))
    vectorl.rotateInRadian(Math.PI / 4) should be(Vectorl(0, sqrtOf2))
    vectorl.rotateInRadian(Math.PI) should be(Vectorl(-1, -1))
    vectorl.rotateInRadian(-Math.PI / 4) should be(Vectorl(sqrtOf2, 0))
  }

  it should "compute angle between two vectors" in {
    vectorl.angleInDegreeBetween(Vectorl(1, 0)) should equal(45.0)
    vectorl.angleInDegreeBetween(Vectorl(0, 1)) should equal(45.0)
  }

}
