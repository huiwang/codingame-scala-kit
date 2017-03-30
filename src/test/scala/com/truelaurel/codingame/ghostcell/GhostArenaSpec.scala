package com.truelaurel.codingame.ghostcell

import com.truelaurel.codingame.engine.GameSimulator
import com.truelaurel.codingame.graph.Edge
import org.scalatest.{FunSpec, Matchers}

/**
  * Created by hwang on 27/03/2017.
  */
class GhostArenaSpec extends FunSpec with Matchers {

  val center = Fac(0, 0, 0, 0, 0)


  val graph = GhostGraph(3, Vector(Edge(0, 1, 1), Edge(0, 2, 1), Edge(1, 2, 1)))
  describe("GhostArenaSpec") {

    it("should resolve battle on neutral fac") {
      val fac1 = Fac(1, 1, 1, 0, 0)
      val fac2 = Fac(2, -1, 1, 0, 0)
      val facs = Vector(center, fac1, fac2)

      val state = GhostCellGameState(
        factories = facs,
        troops = Vector.empty,
        bombs = Vector.empty,
        graph = graph
      )
      val actions = Vector(MoveAction(1, 0, 1))
      val state1 = GhostArena.next(state, actions)
      val state2 = GhostArena.next(state1, Vector.empty)

      state1.fac(0).owner should be(0)
      state2.fac(0).owner should be(1)
    }

    it("should resolve bombs") {
      val fac1 = Fac(1, 1, 33, 0, 0)
      val fac2 = Fac(2, -1, 1, 0, 0)
      val facs = Vector(center, fac1, fac2)

      val state = GhostCellGameState(
        factories = facs,
        troops = Vector.empty,
        bombs = Vector.empty,
        graph = graph
      )
      val state1 = GhostArena.next(state, Vector(BombAction(2, 1)))
      val state2 = GhostArena.next(state1, Vector.empty)
      val state3 = GhostArena.next(state2, Vector(BombAction(2, 1)))
      val state4 = GhostArena.next(state3, Vector.empty)
      state2.fac(1).cyborgs should be(17)
      state4.fac(1).cyborgs should be(7)
      state4.fac(1).again should be(5)
    }

    it("should support multiple move actions") {
      val fac1 = Fac(1, 1, 33, 0, 0)
      val fac2 = Fac(2, -1, 1, 0, 0)
      val facs = Vector(center, fac1, fac2)

      val state = GhostCellGameState(
        factories = facs,
        troops = Vector.empty,
        bombs = Vector.empty,
        graph = graph
      )
      val state1 = GhostArena.next(state, Vector(MoveAction(1, 2, 1), MoveAction(1, 2, 1)))
      val state2 = GhostArena.next(state1, Vector.empty)
      state2.fac(1).cyborgs should be(31)
      state2.fac(2).owner should be(1)
      state2.fac(2).cyborgs should be(1)
    }

    it("should not destroy leaving cyborgs") {
      val fac1 = Fac(1, 1, 33, 0, 0)
      val fac2 = Fac(2, -1, 1, 0, 0)
      val facs = Vector(center, fac1, fac2)

      val state = GhostCellGameState(
        factories = facs,
        troops = Vector.empty,
        bombs = Vector.empty,
        graph = graph
      )
      val state1 = GhostArena.next(state, Vector(BombAction(2, 1), MoveAction(1, 2, 3)))
      val state2 = GhostArena.next(state1, Vector.empty)
      state2.fac(1).cyborgs should be(15)
    }

  }
}
