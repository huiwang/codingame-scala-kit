package com.truelaurel.samplegames.wondev.domain

import com.truelaurel.math.geometry.grid.FastGrid
import com.truelaurel.math.geometry.{Pos, _}
import org.scalatest.{FlatSpec, Matchers}

class FastStateTest extends FlatSpec with Matchers {
  val grid = FastGrid(5)
  val state = FastState(5,
    myUnits = Vector(grid.pos(Pos(1, 1)), grid.pos(Pos(1, 2))),
    opUnits = Vector(grid.pos(Pos(2, 4)), grid.pos(Pos(1, 3))))

  "fast state : build" should "increase height" in {
    val p = grid.pos(Pos(2, 2))
    state.heights(p) shouldBe 0

    val built = state.build(p)

    built.heights(p) shouldBe 1
  }

  "fast state : move" should "move my unit" in {
    val moved = state.move(0, N)

    moved.myUnits(0) shouldBe grid.pos(Pos(1, 1).neighborIn(N))
  }

  "fast state : push" should "push op unit" in {
    val pushed = state.push(1, S, SW)

    pushed.myUnits(1) shouldBe grid.pos(Pos(1, 2))
    pushed.opUnits(1) shouldBe grid.pos(Pos(1, 3).neighborIn(SW))
  }

  "fast state : move" should "move op unit" in {
    val moved = state.copy(nextPlayer = false).move(0, W)

    moved.opUnits(0) shouldBe grid.pos(Pos(2, 4).neighborIn(W))
  }
}
