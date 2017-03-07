package codingame.scala.ghostcell

import codingame.scala.graph.{Edge, Itinerary, ShortestPath}

/**
  * Created by hwang on 26/02/2017.
  */

object GhostCellGrahp {
  def shortestPath(count: Int, undirectedEdges: Vector[Edge]): Map[Int, Map[Int, Itinerary]] = {
    val edges = undirectedEdges.flatMap(edge => Vector(edge, Edge(edge.to, edge.from, edge.distance)))
    ShortestPath.shortestItinearies(count, edges)
  }
}

object GhostCellConstant {
  val MAX_TURN = 20
}

case class Factory(id: Int, owner: Int, cyborgs: Int, production: Int, again: Int) {
  def mine: Boolean = owner == 1

  def other: Boolean = owner == -1

  def inc: Factory = copy(cyborgs = cyborgs - 10, production = production + 1)
}


case class Troop(id: Int, owner: Int, from: Int, to: Int, cyborgs: Int, arrival: Int) {
}

case class Bomb(id: Int, owner: Int, from: Int, to: Int, explosion: Int) {
}

case class GhostCellGameState(itineraries: Map[Int, Map[Int, Itinerary]],
                              factories: Vector[Factory],
                              troops: Vector[Troop],
                              bombs: Vector[Bomb]) {

  val myFacs: Vector[Factory] = factories.filter(_.mine)
  val otherFacs: Vector[Factory] = factories.filter(_.other)

  def dist(from: Int, to: Int): Int = itineraries(from)(to).distance

  def center: Factory = factories(0)

  def fac(id: Int): Factory = factories(id)

}

trait GhostCellAction {
  def command(): String
}

case object WaitAction extends GhostCellAction {
  override def command(): String = "WAIT"
}

sealed case class MoveAction(from: Int, to: Int, cyborgs: Int) extends GhostCellAction {
  override def command(): String = s"MOVE $from $to $cyborgs"
}

sealed case class IncreaseAction(factoryId: Int) extends GhostCellAction {
  override def command(): String = s"INC $factoryId"
}

sealed case class BombAction(from: Int, to: Int) extends GhostCellAction {
  override def command(): String = s"BOMB $from $to"
}

sealed case class MessageAction(msg: String) extends GhostCellAction {
  override def command(): String = s"MSG $msg"
}

