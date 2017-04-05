package com.truelaurel.codingame.csb.common

import com.truelaurel.codingame.collision.Disk
import com.truelaurel.codingame.vectorial.Vectorl

case class CheckPoint(disk: Disk)

case class Pod(disk: Disk, angle: Int, nextCheckPointId: Int) {
}

case class StrikeBackGameState(checkPoints: Vector[CheckPoint], pods: Vector[Pod]) {

  def podsOf(playerId: Int): Vector[Pod] = {
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