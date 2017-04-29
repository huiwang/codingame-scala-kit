package com.truelaurel.codingame.csb.model

import com.truelaurel.codingame.vectorial.{Vectorl, Vectorls}

case class StrikeBackContext(checkPoints: Vector[CheckPoint], previousPods: Vector[Pod])

object StrikeBackContext {
  val podCount = 4
  val podIndices: Vector[Int] = (0 until podCount).toVector
  val me = 0
  val other = 1
}


case class Pod(id: Int, position: Vectorl, speed: Vectorl, angle: Vectorl, goal: Int, mass: Double = 1.0) {
  val radius = 400
}

case class CheckPoint(id: Int, position: Vectorl) {
  val radius = 200
  val speed: Vectorl = Vectorls.origin
}

case class StrikeBackState(checkPoints: Vector[CheckPoint],
                           pods: Vector[Pod]) {
  def podsOf(playerId: Int): Vector[Pod] = {
    if (playerId == 0) {
      pods.take(2)
    } else {
      pods.takeRight(2)
    }
  }

  def checkPoint(goal: Int): Vectorl = checkPoints(goal % checkPoints.size).position

}

trait StrikeBackAction {
  def id: Int
}

sealed case class Thrust(id: Int, target: Vectorl, thrust: Int) extends StrikeBackAction {
  override def toString: String = s"${target.x.toInt} ${target.y.toInt} $thrust"
}

sealed case class AngleThrust(id: Int, position: Vectorl, angle: Vectorl, rotate: Double, thrust: Double) extends StrikeBackAction {
  override def toString: String = Thrust(id, position + angle.rotateInDegree(rotate) * 100000, thrust.toInt).toString
}

sealed case class Shield(id: Int) extends StrikeBackAction {
  override def toString: String = "SHIELD"
}

sealed case class Boost(id: Int) extends StrikeBackAction {
  override def toString: String = "BOOST"
}