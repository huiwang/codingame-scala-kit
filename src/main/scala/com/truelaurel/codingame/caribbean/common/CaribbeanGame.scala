package com.truelaurel.codingame.caribbean.common

import com.truelaurel.codingame.hexagons.{Cube, Offset}

/**
  * Created by hwang on 14/04/2017.
  */

object CaribbeanConstants {
  val width = 23
  val hight = 21
}

case class CaribbeanContext(lastMine: Map[Int, Int], lastFire: Map[Int, Int])

case class Ship(id: Int, position: Offset, orientation: Int, speed: Int, rums: Int, owner: Int) {
  lazy val cube: Cube = position.toCube
  lazy val center: Cube = cube
  lazy val front: Cube = center.neighbor(orientation)
  lazy val back: Cube = center.neighbor((orientation + 3) % 6)
}

case class Barrel(id: Int, position: Offset, rums: Int) {
  lazy val cube: Cube = position.toCube
}

case class Ball(id: Int, target: Offset, owner: Int, land: Int)

case class Mine(id: Int, position: Offset) {
  lazy val cube: Cube = position.toCube
}

case class CaribbeanState(context: CaribbeanContext,
                          ships: Vector[Ship],
                          barrels: Vector[Barrel],
                          balls: Vector[Ball],
                          mines: Vector[Mine],
                          turn: Int) {
  def shipsOf(owner: Int): Vector[Ship] = ships.filter(_.owner == owner)
}

trait CaribbeanAction {
  def shipId: Int
}

sealed case class Move(shipId: Int, offset: Offset) extends CaribbeanAction {
  override def toString: String = s"MOVE ${offset.x} ${offset.y}"
}

sealed case class Slower(shipId: Int) extends CaribbeanAction {
  override def toString: String = "SLOWER"
}

sealed case class Faster(shipId: Int) extends CaribbeanAction {
  override def toString: String = "FASTER"
}

sealed case class Port(shipId: Int) extends CaribbeanAction {
  override def toString: String = "PORT"
}

sealed case class Starboard(shipId: Int) extends CaribbeanAction {
  override def toString: String = "STARBOARD"
}

sealed case class Wait(shipId: Int) extends CaribbeanAction {
  override def toString: String = "WAIT"
}

sealed case class Fire(shipId: Int, offset: Offset) extends CaribbeanAction {
  override def toString: String = s"MOVE ${offset.x} ${offset.y}"
}

sealed case class MineAction(shipId: Int) extends CaribbeanAction {
  override def toString: String = s"MINE"
}
