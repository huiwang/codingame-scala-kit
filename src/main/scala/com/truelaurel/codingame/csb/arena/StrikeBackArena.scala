package com.truelaurel.codingame.csb.arena

import com.truelaurel.codingame.csb.model._
import com.truelaurel.codingame.game.{Draw, GameArena, GameResult}

/**
  * Created by hwang on 02/04/2017.
  */
object StrikeBackArena extends GameArena[StrikeBackState, StrikeBackAction] {

  override def next(state: StrikeBackState, actions: Vector[StrikeBackAction]): StrikeBackState = {
    val steeredPods = for {
      action <- actions
      pod = state.pods(action.id)
    } yield action match {
      case Thrust(_, target, thrust) =>
        val pivoted = pod.angle.pivotTo(target - pod.position, 18.0)
        pod.copy(speed = pod.speed + pivoted * thrust)
      case AngleThrust(_, _, angle, rotate, thrust) =>
        val rotated = angle.rotateInDegree(rotate)
        pod.copy(speed = pod.speed + rotated * thrust, angle = rotated)
      case _ => pod
    }

    val movedPods = StrikeBackCollisionSimulation.simulate(state.checkPoints, steeredPods, 1.0)

    val slowed = movedPods.map(p => p.copy(speed = p.speed * .85))

    StrikeBackState(state.checkPoints, slowed)
  }

  override def judge(state: StrikeBackState): GameResult = {
    Draw
  }

}