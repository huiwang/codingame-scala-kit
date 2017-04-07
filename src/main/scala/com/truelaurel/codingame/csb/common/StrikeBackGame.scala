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
  def command(): String
}

sealed case class Thrust(target: Vectorl, thrust: Int) extends PodAction {
  override def command(): String = s"${target.x.toInt} ${target.y.toInt} $thrust"
}

case object Shield extends PodAction {
  override def command(): String = "SHIELD"
}

case object Boost extends PodAction {
  override def command(): String = "BOOST"
}