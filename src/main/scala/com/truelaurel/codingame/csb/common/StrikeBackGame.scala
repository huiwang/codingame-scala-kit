package com.truelaurel.codingame.csb.common

import com.truelaurel.codingame.collision.Disk
import com.truelaurel.codingame.vectorial.Vectorl

object CSBConstant {
  val cpRadius = 200
  val podRadius = 400
  val axisX: Vectorl = Vectorl(1, 0)
  val podCount = 4
  val podIndices: Vector[Int] = (0 until podCount).toVector
}

case class StrikeBackGameState(checkPoints: Vector[Disk],
                               pods: Vector[Disk],
                               angles: Vector[Vectorl],
                               nextCPs: Vector[Int]) {
  def podsOf(playerId: Int): Vector[Disk] = {
    if (playerId == 0) {
      pods.take(2)
    } else {
      pods.takeRight(2)
    }
  }

}

trait PodAction {
}

sealed case class Thrust(target: Vectorl, thrust: Int) extends PodAction {
  override def toString: String = s"${target.x.toInt} ${target.y.toInt} $thrust"
}

sealed case class AngleThrust(position: Vectorl, angle: Vectorl, rotate: Double, thrust: Double) extends PodAction {
  override def toString: String = Thrust(position + angle.rotateInDegree(rotate) * 100000, thrust.toInt).toString
}

case object Shield extends PodAction {
  override def toString: String = "SHIELD"
}

case object Boost extends PodAction {
  override def toString: String = "BOOST"
}