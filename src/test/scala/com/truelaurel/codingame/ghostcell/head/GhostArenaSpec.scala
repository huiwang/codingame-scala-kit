package com.truelaurel.codingame.ghostcell.head

import com.truelaurel.codingame.engine.{GameSimulator, WinKO}
import com.truelaurel.codingame.ghostcell.offline.GhostArena
import com.truelaurel.codingame.ghostcell.best.BestGhostCellPlayer
import com.truelaurel.codingame.ghostcell.common._
import com.truelaurel.codingame.ghostcell.{best, head}
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

    it("should simulate an entire game") {
      val from = GhostCellGameState(Vector(Fac(0, 0, 0, 0, 0), Fac(1, 1, 25, 3, 0), Fac(2, -1, 25, 3, 0), Fac(3, 0, 6, 2, 0), Fac(4, 0, 6, 2, 0), Fac(5, 0, 0, 0, 0), Fac(6, 0, 0, 0, 0), Fac(7, 0, 0, 1, 0), Fac(8, 0, 0, 1, 0), Fac(9, 0, 0, 0, 0), Fac(10, 0, 0, 0, 0), Fac(11, 0, 0, 0, 0), Fac(12, 0, 0, 0, 0), Fac(13, 0, 6, 2, 0), Fac(14, 0, 6, 2, 0)), Vector(), Vector(), 1, Map(1 -> 2, -1 -> 2), GhostGraph(15, Vector(Edge(0, 1, 1), Edge(0, 2, 1), Edge(0, 3, 3), Edge(0, 4, 3), Edge(0, 5, 4), Edge(0, 6, 4), Edge(0, 7, 4), Edge(0, 8, 4), Edge(0, 9, 7), Edge(0, 10, 7), Edge(0, 11, 7), Edge(0, 12, 7), Edge(0, 13, 1), Edge(0, 14, 1), Edge(1, 2, 4), Edge(1, 3, 3), Edge(1, 4, 4), Edge(1, 5, 2), Edge(1, 6, 7), Edge(1, 7, 3), Edge(1, 8, 6), Edge(1, 9, 6), Edge(1, 10, 9), Edge(1, 11, 4), Edge(1, 12, 9), Edge(1, 13, 1), Edge(1, 14, 3), Edge(2, 3, 4), Edge(2, 4, 3), Edge(2, 5, 7), Edge(2, 6, 2), Edge(2, 7, 6), Edge(2, 8, 3), Edge(2, 9, 9), Edge(2, 10, 6), Edge(2, 11, 9), Edge(2, 12, 4), Edge(2, 13, 3), Edge(2, 14, 1), Edge(3, 4, 7), Edge(3, 5, 3), Edge(3, 6, 7), Edge(3, 7, 1), Edge(3, 8, 8), Edge(3, 9, 4), Edge(3, 10, 11), Edge(3, 11, 6), Edge(3, 12, 9), Edge(3, 13, 1), Edge(3, 14, 5), Edge(4, 5, 7), Edge(4, 6, 3), Edge(4, 7, 8), Edge(4, 8, 1), Edge(4, 9, 11), Edge(4, 10, 4), Edge(4, 11, 9), Edge(4, 12, 6), Edge(4, 13, 5), Edge(4, 14, 1), Edge(5, 6, 10), Edge(5, 7, 1), Edge(5, 8, 10), Edge(5, 9, 3), Edge(5, 10, 13), Edge(5, 11, 2), Edge(5, 12, 12), Edge(5, 13, 2), Edge(5, 14, 7), Edge(6, 7, 10), Edge(6, 8, 1), Edge(6, 9, 13), Edge(6, 10, 3), Edge(6, 11, 12), Edge(6, 12, 2), Edge(6, 13, 7), Edge(6, 14, 2), Edge(7, 8, 9), Edge(7, 9, 2), Edge(7, 10, 13), Edge(7, 11, 4), Edge(7, 12, 11), Edge(7, 13, 1), Edge(7, 14, 7), Edge(8, 9, 13), Edge(8, 10, 2), Edge(8, 11, 11), Edge(8, 12, 4), Edge(8, 13, 7), Edge(8, 14, 1), Edge(9, 10, 16), Edge(9, 11, 4), Edge(9, 12, 15), Edge(9, 13, 5), Edge(9, 14, 10), Edge(10, 11, 15), Edge(10, 12, 4), Edge(10, 13, 10), Edge(10, 14, 5), Edge(11, 12, 15), Edge(11, 13, 5), Edge(11, 14, 9), Edge(12, 13, 9), Edge(12, 14, 5), Edge(13, 14, 4))))
      val result = GameSimulator.play(from, GhostArena, Vector(GhostCellPlayer(1), BestGhostCellPlayer(-1)))
      result should be(WinKO)
    }

  }
}
