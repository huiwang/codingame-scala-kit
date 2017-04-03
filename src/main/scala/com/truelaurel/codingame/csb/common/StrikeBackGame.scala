package com.truelaurel.codingame.csb.common

case class CheckPoint(x: Int, y: Int)

case class Pod(x: Int, y: Int, vx: Int, vy: Int, angle: Int, nextCheckPointId: Int) {
}

case class StrikeBackGameState(checkPoints: Vector[CheckPoint], pods: Vector[Vector[Pod]]) {

}

trait PodAction {
  def command(): String
}

sealed case class Thrust(x: Int, y: Int, thrust: Int) extends PodAction {
  override def command(): String = s"$x $y $thrust"
}

case object Shield extends PodAction {
  override def command(): String = "SHIELD"
}

case object Boost extends PodAction {
  override def command(): String = "BOOST"
}

