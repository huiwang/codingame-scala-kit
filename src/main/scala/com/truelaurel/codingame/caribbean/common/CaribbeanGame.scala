package com.truelaurel.codingame.caribbean.common

import com.truelaurel.codingame.hexagons.{Cube, Offset}

/**
  * Created by hwang on 14/04/2017.
  */

case class CaribbeanContext(lastMine: Map[Int, Int], lastFire: Map[Int, Int]) {
}

object CaribbeanContext {
  val highMineDamage = 25
  val lowMineDamage = 10
  val width = 23
  val height = 21
  val cubes: Vector[Cube] = (for {
    x <- 0 until width
    y <- 0 until height
  } yield Offset(x, y).toCube).toVector


  val cubeToNeighbors: Map[Cube, Vector[Cube]] = cubes
    .map(cube =>
      cube -> (0 to 5).map(cube.neighbor).filter(cubes.contains).toVector)
    .toMap

  def apply(): CaribbeanContext = CaribbeanContext(Map.empty, Map.empty)
}

case class Ship(id: Int, position: Offset, orientation: Int, speed: Int, rums: Int, owner: Int) {
  lazy val cube: Cube = position.toCube
  lazy val center: Cube = cube
  lazy val bow: Cube = center.neighbor(orientation)
  lazy val stern: Cube = center.neighbor((orientation + 3) % 6)
  lazy val cubeSet = Set(center, bow, stern)
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
