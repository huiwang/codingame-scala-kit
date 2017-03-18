package codingame.scala.kit.ghostcell

import codingame.scala.kit.graph.{Edge, Iti}
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by hwang on 28/02/2017.
  */
class GhostCellPlayerTest extends FlatSpec with Matchers {

  behavior of "GhostCellPlayer$Test"

  it should "study" in {
    GhostCellPlayer.reactTo(GhostCellGameState(Vector(Fac(0,1,11,3,0), Fac(1,1,2,1,0), Fac(2,-1,2,1,0), Fac(3,1,0,2,3), Fac(4,-1,2,2,0), Fac(5,1,4,3,0), Fac(6,-1,6,3,3), Fac(7,1,2,0,0), Fac(8,-1,0,0,0), Fac(9,1,0,2,5), Fac(10,-1,4,3,0)),Vector(Troop(85,1,3,0,2,1), Troop(86,1,7,5,1,1), Troop(89,-1,6,10,6,1), Troop(95,-1,4,6,2,2), Troop(97,-1,8,6,1,2), Troop(98,-1,10,6,12,2), Troop(99,-1,4,0,10,2), Troop(103,1,9,3,10,1), Troop(104,1,9,7,1,2), Troop(105,1,5,7,4,3), Troop(106,1,0,5,2,1), Troop(107,-1,10,2,11,1), Troop(108,-1,4,10,2,1)),Vector(Bomb(72,1,0,10,1,12)),15,Vector(Edge(0,1,7), Edge(0,2,7), Edge(0,3,3), Edge(0,4,3), Edge(0,5,1), Edge(0,6,1), Edge(0,7,5), Edge(0,8,5), Edge(0,9,4), Edge(0,10,4), Edge(1,2,15), Edge(1,3,4), Edge(1,4,11), Edge(1,5,5), Edge(1,6,9), Edge(1,7,1), Edge(1,8,13), Edge(1,9,1), Edge(1,10,12), Edge(2,3,11), Edge(2,4,4), Edge(2,5,9), Edge(2,6,5), Edge(2,7,13), Edge(2,8,1), Edge(2,9,12), Edge(2,10,1), Edge(3,4,8), Edge(3,5,3), Edge(3,6,5), Edge(3,7,4), Edge(3,8,9), Edge(3,9,1), Edge(3,10,8), Edge(4,5,5), Edge(4,6,3), Edge(4,7,9), Edge(4,8,4), Edge(4,9,8), Edge(4,10,1), Edge(5,6,4), Edge(5,7,3), Edge(5,8,8), Edge(5,9,3), Edge(5,10,6), Edge(6,7,8), Edge(6,8,3), Edge(6,9,6), Edge(6,10,3), Edge(7,8,12), Edge(7,9,2), Edge(7,10,10), Edge(8,9,10), Edge(8,10,2), Edge(9,10,9)))
    ) should be(Vector.empty)
  }

}
