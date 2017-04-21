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
  val lowBallDamage = 25
  val highBallDamage = 50
  val fireMaxDistance = 10
  val width = 23
  val height = 21
  val me = 1
  val other = 0
  private val orientations = (0 until 6).toVector
  val cubes: Vector[Cube] = (for {
    x <- 0 until width
    y <- 0 until height
  } yield Offset(x, y).toCube).toVector
  private val cubeInfo: Map[Cube, (Set[Cube], Map[Int, Set[Cube]], Map[Cube, Double])] = cubes
    .map(cube => {
      val neighbors = (0 to 5).map(cube.neighbor).filter(cubes.contains).toSet
      val oriToZone = orientations.map(ori =>
        ori -> Set(cube, cube.neighbor(ori), cube.neighbor((ori + 3) % 6))
      ).toMap
      val angles = cubes.map(ic => ic -> cube.toOffset.angle(ic.toOffset)
      ).toMap
      cube -> (neighbors, oriToZone, angles)
    })
    .toMap

  def shipZone(ship: Ship): Set[Cube] = cubeInfo(ship.center)._2(ship.orientation)

  def apply(): CaribbeanContext = CaribbeanContext(Map.empty, Map.empty)

  def angle(ship: Ship, target: Cube): Double = cubeInfo(ship.center)._3(target)

  def toCube(offset: Offset): Cube = offset.toCube

  def neighbors(cube: Cube): Set[Cube] = cubeInfo(cube)._1
}

case class Ship(id: Int, position: Offset, orientation: Int, speed: Int, rums: Int, owner: Int) {
  def center: Cube = CaribbeanContext.toCube(position)

  def bow: Cube = center.neighbor(orientation)

  def stern: Cube = center.neighbor((orientation + 3) % 6)
}

case class Barrel(id: Int, position: Offset, rums: Int) {
  def cube: Cube = CaribbeanContext.toCube(position)
}

case class Ball(id: Int, target: Offset, owner: Int, land: Int) {
  def cube: Cube = CaribbeanContext.toCube(target)

}

case class Mine(id: Int, position: Offset) {
  def cube: Cube = CaribbeanContext.toCube(position)
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
  override def toString: String = s"FIRE ${offset.x} ${offset.y}"
}

sealed case class MineAction(shipId: Int) extends CaribbeanAction {
  override def toString: String = s"MINE"
}
