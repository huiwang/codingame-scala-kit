package com.truelaurel.codingame.ghostcell

import com.truelaurel.codingame.graph.{Edge, Iti, ShortestPath}

/**
  * Created by hwang on 26/02/2017.
  */

object GhostCellConstant {
  val MAX_TURN = 20
}

case class Fac(id: Int, owner: Int, cyborgs: Int, production: Int, again: Int) {
  def mine: Boolean = owner == 1

  def other: Boolean = owner == -1

}

case class Entity(entityId: Int, entityType: String, arg1: Int, arg2: Int, arg3: Int, arg4: Int, arg5: Int)

case class Troop(id: Int, owner: Int, from: Int, to: Int, cyborgs: Int, arrival: Int) {
}

case class Bomb(id: Int, owner: Int, from: Int, to: Int, explosion: Int, birth: Int = 0) {
}

case class GhostCellGameState(factories: Vector[Fac],
                              troops: Vector[Troop],
                              bombs: Vector[Bomb],
                              turn: Int = 1,
                              bombBudget: Map[Int, Int] = Map(1 -> 2, -1 -> 2),
                              graph: GhostGraph) {

  val facValue: Map[Int, Double] = factories.map(fac => {
    val passThroughCount = graph.passThroughSegments.count(segment => segment.contains(fac.id))
    fac.id -> (fac.production + 0.01 * Math.pow(passThroughCount, 0.5) + 0.1)
  }).toMap

  def myFacs: Vector[Fac] = factories.filter(_.mine)

  def otherFacs: Vector[Fac] = factories.filter(_.other)

  def dist(from: Int, to: Int): Int = graph.itineraries(from)(to).distance

  def transferFac(from: Int, to: Int): Int = graph.itineraries(from)(to).path.tail.head

  def directDist(from: Int, to: Int): Int = if (from == to) 0 else graph.directDistances((from, to))

  def center: Fac = factories(0)

  def fac(id: Int): Fac = factories(id)

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

