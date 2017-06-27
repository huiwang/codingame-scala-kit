package com.truelaurel.samplegames.wondev.domain

import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class FastGridTest extends FlatSpec with Matchers with PropertyChecks {
  "FastGrid" should "have injective position" in {
    forAll(genGrid) { grid =>
      forAll(Gen.chooseNum(0, grid.size * grid.size - 1)) { p =>
        val pos = grid.pos(p)
        grid.pos(pos) shouldBe p
      }
    }
  }

  "FastGrid" should "have surjective position" in {
    forAll(genGrid) { grid =>
      forAll(Gen.chooseNum(0, grid.size * grid.size - 1)) { p =>
        forAll(Gen.chooseNum(0, grid.size * grid.size - 1)) { q =>
          whenever(p != q) {
            val pos = grid.pos(p)
            val qos = grid.pos(q)
            pos should not be qos
          }
        }
      }
    }
  }

  def genGrid: Gen[FastGrid] =
    for {
      size <- Gen.chooseNum(minT = 3, maxT = 10)
    } yield new FastGrid(size)

}
