package com.truelaurel.codingame.caribbean.common

import com.truelaurel.codingame.hexagons.Cube

/**
  * Created by hwang on 14/04/2017.
  */

case class CaribbeanContext()

case class Ship(id: Int, x: Int, y: Int, orientation: Int, speed: Int, rums: Int, owner: Int) {
  lazy val front: Cube = Cube(x,y).neighbor(orientation)
  lazy val back = Cube(x, y).neighbor((orientation + 3) % 6)
  lazy val center = Cube(x, y)
}

case class Barrel(id: Int, x: Int, y: Int, rums: Int)

case class Ball(id: Int, x: Int, y: Int, owner: Int, land: Int)

case class Mine(id: Int, x: Int, y: Int)

case class CaribbeanState(context: CaribbeanContext,
                          ships: Vector[Ship],
                          barrels: Vector[Barrel],
                          balls: Vector[Ball],
                          mines: Vector[Mine],
                          turn: Int) {
  def shipsOf(playerId : Int): Vector[Ship] = ships.filter(_.owner == playerId)
}

trait CaribbeanAction {
}

sealed case class Move(x: Int, y: Int) extends CaribbeanAction {
  override def toString: String = s"MOVE $x $y"
}

object Slower extends CaribbeanAction {
  override def toString: String = "SLOWER"
}

object Wait extends CaribbeanAction {
  override def toString: String = "WAIT"
}

sealed case class Fire(x :Int, y : Int) extends CaribbeanAction {
  override def toString: String = s"FIRE $x $y"
}
