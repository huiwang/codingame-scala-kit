package com.truelaurel.codingame.ghostcell

import com.truelaurel.codingame.graph.{Edge, Iti}
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by hwang on 28/02/2017.
  */
class GhostCellPlayerTest extends FlatSpec with Matchers {

  behavior of "GhostCellPlayer$Test"

  ignore should "study the behavoir by copying game state from console" in {
    GhostCellPlayer.reactTo(GhostCellGameState(factories = Vector.empty, troops = Vector.empty, bombs = Vector.empty, turn = 0, graph = GhostGraph(0, Vector.empty))) should be(Vector.empty)
  }

}
