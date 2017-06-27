package com.truelaurel.samplegames.wondev.domain

import com.truelaurel.math.geometry.Pos
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class FastGridTest extends FlatSpec with Matchers with PropertyChecks {
  "FastGrid" should "have some position" in {
    forAll(genGrid) { g =>
      forAll(Gen.chooseNum(0, g.size*g.size -1)) {p=>
        g.positions(p)
//        g.
      }
    }
  }

  def genGrid: Gen[FastGrid] =
    for {
      size <- Gen.chooseNum(minT = 3, maxT = 10)
    } yield new FastGrid(size)

//  def genPos(size:Int): Gen[Pos] =
//    for {
//      size <- Gen.chooseNum(minT = 3, maxT = 10)
//    } yield new FastGrid(size)
}
