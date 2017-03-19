package codingame.scala.kit.ghostcell

import codingame.scala.kit.graph.{Edge, Iti}
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by hwang on 28/02/2017.
  */
class GhostCellPlayerTest extends FlatSpec with Matchers {

  behavior of "GhostCellPlayer$Test"

  ignore should "study the behavoir by copying game state from console" in {
    GhostCellPlayer.reactTo(GhostCellGameState(Vector(Fac(0,1,0,0,0), Fac(1,1,3,0,0), Fac(2,-1,4,0,0), Fac(3,1,10,2,0), Fac(4,-1,2,2,0), Fac(5,1,0,0,0), Fac(6,0,0,0,0), Fac(7,-1,1,1,3), Fac(8,-1,8,1,0), Fac(9,0,0,0,0), Fac(10,0,0,0,0), Fac(11,1,12,2,0), Fac(12,-1,0,2,5)),Vector(Troop(38,1,3,7,2,2), Troop(39,-1,4,8,7,2), Troop(40,-1,12,2,2,1), Troop(42,-1,4,2,2,2), Troop(43,-1,12,2,2,2)),Vector(Bomb(19,-1,2,-1,-1,2), Bomb(26,1,0,4,1,5)),9,Vector(Edge(0,1,2), Edge(0,2,2), Edge(0,3,5), Edge(0,4,5), Edge(0,5,5), Edge(0,6,5), Edge(0,7,2), Edge(0,8,2), Edge(0,9,7), Edge(0,10,7), Edge(0,11,4), Edge(0,12,4), Edge(1,2,5), Edge(1,3,2), Edge(1,4,8), Edge(1,5,2), Edge(1,6,9), Edge(1,7,2), Edge(1,8,4), Edge(1,9,4), Edge(1,10,10), Edge(1,11,2), Edge(1,12,7), Edge(2,3,8), Edge(2,4,2), Edge(2,5,9), Edge(2,6,2), Edge(2,7,4), Edge(2,8,2), Edge(2,9,10), Edge(2,10,4), Edge(2,11,7), Edge(2,12,2), Edge(3,4,11), Edge(3,5,1), Edge(3,6,11), Edge(3,7,3), Edge(3,8,7), Edge(3,9,4), Edge(3,10,13), Edge(3,11,4), Edge(3,12,9), Edge(4,5,11), Edge(4,6,1), Edge(4,7,7), Edge(4,8,3), Edge(4,9,13), Edge(4,10,4), Edge(4,11,9), Edge(4,12,4), Edge(5,6,12), Edge(5,7,5), Edge(5,8,7), Edge(5,9,1), Edge(5,10,14), Edge(5,11,2), Edge(5,12,11), Edge(6,7,7), Edge(6,8,5), Edge(6,9,14), Edge(6,10,1), Edge(6,11,11), Edge(6,12,2), Edge(7,8,5), Edge(7,9,7), Edge(7,10,8), Edge(7,11,5), Edge(7,12,4), Edge(8,9,8), Edge(8,10,7), Edge(8,11,4), Edge(8,12,5), Edge(9,10,16), Edge(9,11,2), Edge(9,12,12), Edge(10,11,12), Edge(10,12,2), Edge(11,12,10)))
    ) should be(Vector.empty)
  }

}
