package com.truelaurel.codingame.caribbean.common

import com.truelaurel.codingame.hexagons.{Cube, Offset}

/**
  * Created by hwang on 14/04/2017.
  */

case class CaribbeanContext(mines: Map[Int, Mine], lastFire: Map[Int, Int]) {
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
  val maxRums = 150
  private val orientations = (0 until 6).toVector
  private val cubes: Vector[Cube] = (for {
    x <- 0 until width
    y <- 0 until height
  } yield Offset(x, y).toCube).toVector
  private val cubeInfo: Map[Cube, (Set[Cube], Map[Int, Set[Cube]], Vector[Cube], Map[Cube, Int])] = cubes
    .map(cube => {
      val neighbors = (0 to 5).map(cube.neighbor).filter(cubes.contains).toSet
      val oriToZone = orientations.map(ori =>
        ori -> Set(cube.neighbor(ori), cube.neighbor((ori + 3) % 6))
      ).toMap
      val reachable = cubes.filter(_.distanceTo(cube) <= 5)
      val angles = cubes.map(ic => ic -> cube.toOffset.angle(ic.toOffset).ceil.toInt).toMap
      cube -> (neighbors, oriToZone, reachable, angles)
    }).toMap

  def shipZone(cube: Cube, orientation: Int): Set[Cube] = cubeInfo(cube)._2(orientation)

  def apply(): CaribbeanContext = CaribbeanContext(Map.empty, Map.empty)

  def reachable(cube: Cube): Vector[Cube] = cubeInfo(cube)._3

  def toCube(offset: Offset): Cube = offset.toCube

  def neighbors(cube: Cube): Set[Cube] = cubeInfo(cube)._1

  def angle(one: Cube, other: Cube) : Int = cubeInfo(one)._4(other)

  def inside(offset: Offset) : Boolean = {
    offset.x >= 0 && offset.x < width && offset.y >= 0 && offset.y < height
  }
}

case class Ship(id: Int, position: Offset, orientation: Int, speed: Int, rums: Int, owner: Int) {
  val center: Cube = CaribbeanContext.toCube(position)

  val bowAndStern: Set[Cube] = CaribbeanContext.shipZone(center, orientation)

  def zone: Set[Cube] = bowAndStern + center

  val bow: Cube = center.neighbor(orientation)

  val stern: Cube = center.neighbor((orientation + 3) % 6)

  def mine : Cube = stern.neighbor((orientation + 3) % 6)

  def nextBow: Cube = speed match {
    case 0 => bow
    case 1 => bow.neighbor(orientation)
    case 2 => bow.neighbor(orientation).neighbor(orientation)
  }

  def nextCenter: Cube = speed match {
    case 0 => center
    case 1 => bow
    case 2 => bow.neighbor(orientation)
  }

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
                          ships: Map[Int, Ship],
                          barrels: Map[Int, Barrel],
                          balls: Map[Int, Ball],
                          mines: Map[Int, Mine],
                          turn: Int) {
  def shipsOf(owner: Int): Vector[Ship] = ships.values.filter(_.owner == owner).toVector.sortBy(_.id)
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
