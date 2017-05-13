package com.truelaurel.codingame.csb.model

import com.truelaurel.codingame.csb.analysis.PodAnalysis
import com.truelaurel.codingame.vectorial.{Vectorl, Vectorls}

case class StrikeBackContext(checkPoints: Vector[CheckPoint], previousPods: Vector[Pod], shieldCoolDown: Vector[Int])

object StrikeBackContext {
  val podCount = 4
  val podIndices: Vector[Int] = (0 until podCount).toVector
  val me = 0
  val other = 1
  val shieldCd = 3
}


case class Pod(id: Int, position: Vectorl, speed: Vectorl, angle: Vectorl, goal: Int, mass: Double = 1.0) {
  val radius = 400
}

case class CheckPoint(id: Int, position: Vectorl) {
  val radius = 200
  val speed: Vectorl = Vectorls.origin
}

case class StrikeBackState(context: StrikeBackContext,
                           pods: Vector[Pod],
                           turn: Int) {
  def podsOf(playerId: Int): Vector[Pod] = {
    if (playerId == 0) {
      pods.take(2)
    } else {
      pods.takeRight(2)
    }
  }

  //first is defenser and second is racer
  def role(playerId: Int): Vector[Pod] = podsOf(playerId)
    .sortBy(p => (p.goal, -PodAnalysis.podToGoalDistance(p, this)))

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

sealed case class Shield(id: Int, position: Vectorl, angle: Vectorl, rotate: Double) extends StrikeBackAction {
  val target = position + angle.rotateInDegree(rotate) * 100000

  override def toString: String = s"${target.x.toInt} ${target.y.toInt} SHIELD"
}

sealed case class Boost(id: Int) extends StrikeBackAction {
  override def toString: String = "BOOST"
}