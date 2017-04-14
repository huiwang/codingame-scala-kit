package com.truelaurel.codingame.caribbean.common

/**
  * Created by hwang on 14/04/2017.
  */

case class CaribbeanContext()

case class Ship(id : Int, x: Int, y: Int, rotation: Int, speed: Int, rums: Int, owner: Int)

case class Barrel(id : Int, x: Int, y: Int, rums: Int)

case class CaribbeanState(context: CaribbeanContext,
                          ships: Vector[Ship],
                          barrels: Vector[Barrel],
                          turn: Int)

trait CaribbeanAction {
}

sealed case class Move(x: Int, y: Int) extends CaribbeanAction{
  override def toString: String = s"MOVE $x $y"
}

object Slower extends CaribbeanAction {
  override def toString: String = "SLOWER"
}

object Wait extends CaribbeanAction {
  override def toString: String = "WAIT"
}
