package com.truelaurel.samplegames.wondev.analysis

import com.truelaurel.math.geometry.Pos
import com.truelaurel.samplegames.wondev.domain.{WondevContext, WondevState}
import com.truelaurel.samplegames.wondev.io.WondevController
import org.scalatest.{FlatSpec, Matchers}

class WondevAnalysisTest extends FlatSpec with Matchers {
  val rows = Seq(
    ".000.",
    ".020.",
    ".0101",
    ".0200",
    ".000."
  )
  val state = WondevState(
    WondevContext(size = 5, unitsperplayer = 2),
    27,
    WondevController.parseHeights(rows),
    myUnits = List(Pos(4, 2), Pos(2, 3)),
    opUnits = List(Pos(3, 2), Pos(2, 0)))

  "WondevAnalysis" should "evaluate unit" in {
    WondevAnalysis.evaluate(Pos(2, 2), state) shouldBe (8 * 100 + 10)
  }
}
