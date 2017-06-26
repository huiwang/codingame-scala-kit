package com.truelaurel.samplegames.wondev.arena

import com.truelaurel.math.geometry._
import com.truelaurel.samplegames.wondev.domain.{MoveBuild, MovePush, WondevContext, WondevState}
import com.truelaurel.samplegames.wondev.io.WondevController
import org.scalatest.{FlatSpec, Matchers}

class WondevArenaTest extends FlatSpec with Matchers {
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

  "WondevArena" should "compute next state after a move&build" in {
    val n = WondevArena.next(state, Vector(MoveBuild(0, S, N)))
    n.myUnits.head shouldBe Pos(4, 3)
    n.heightMap(Pos(4, 2)) shouldBe 2
  }

  it should "compute next state after a move&push" in {
    val n = WondevArena.next(state, Vector(MovePush(0, W, W)))
    n.myUnits.head shouldBe Pos(4, 2)
    n.opUnits.head shouldBe Pos(2, 2)
    n.heightMap(Pos(3, 2)) shouldBe 1
  }

}
