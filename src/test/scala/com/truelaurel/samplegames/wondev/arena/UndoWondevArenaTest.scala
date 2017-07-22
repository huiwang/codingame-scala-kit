package com.truelaurel.samplegames.wondev.arena

import com.truelaurel.math.geometry.Pos
import com.truelaurel.samplegames.wondev.domain.{FastWondevState, MoveBuild}
import org.scalatest.{FlatSpec, Matchers}

class UndoWondevArenaTest extends FlatSpec with Matchers {

  behavior of "UndoWondevArenaTest"

  val state = new FastWondevState(
    size = 5,
    units = Array(Pos(0, 0), Pos(4, 0), Pos(0, 4), Pos(4, 4)),
    height = Array(
      Array(0, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 0)
    ),
    nextPlayer = true
  )

  it should "next" in {
    val copied = state.copy()
    val simulated = UndoWondevArena.next(copied, MoveBuild(0, Pos(1, 1), Pos(1, 0)))
    val expected = new FastWondevState(
      size = 5,
      units = Array(Pos(1, 1), Pos(4, 0), Pos(0, 4), Pos(4, 4)),
      height = Array(
        Array(0, 0, 0, 0, 0),
        Array(1, 0, 0, 0, 0),
        Array(0, 0, 0, 0, 0),
        Array(0, 0, 0, 0, 0),
        Array(0, 0, 0, 0, 0)
      ),
      nextPlayer = false
    )
    simulated should be(expected)
    simulated.readable.isFree(Pos(0,0)) should be(true)
    simulated.readable.isFree(Pos(1,1)) should be(false)

    simulated.undoable.undo()

    state should be(simulated)
    simulated.readable.isFree(Pos(0,0)) should be(false)
    simulated.readable.isFree(Pos(1,1)) should be(true)
  }

}
