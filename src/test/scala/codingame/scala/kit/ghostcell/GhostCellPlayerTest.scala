package codingame.scala.kit.ghostcell

import codingame.scala.kit.graph.{Edge, ShortestPath}
import codingame.scala.kit.ghostcell._
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by hwang on 28/02/2017.
  */
class GhostCellPlayerTest extends FlatSpec with Matchers {

  behavior of "GhostCellPlayer$Test"

  it should "find a good move for simple case" in {
    val state = GhostCellGameState(ShortestPath.shortestItinearies(3, Vector(
          Edge(0, 1, 1),
          Edge(0, 2, 1),
          Edge(1, 0, 1),
          Edge(1, 2, 2),
          Edge(2, 0, 1),
          Edge(2, 1, 2)
        )), Vector(
            Factory(id = 0, owner = 1, cyborgs = 0, production = 0, again = 0),
            Factory(id = 1, owner = -1, cyborgs = 0, production = 1, again = 0),
            Factory(id = 2, owner = 1, cyborgs = 4, production = 0, again = 0)
          ), Vector.empty, Vector.empty)
    GhostCellPlayer.reactTo(state) should be(Vector(MoveAction(2, 1, 4), BombAction(0, 1)))
  }

}
