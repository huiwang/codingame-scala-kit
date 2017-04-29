package com.truelaurel.codingame.csb.arena

import com.truelaurel.codingame.csb.io.StrikeBackController
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
      case Shield(_, _, angle, rotate) =>
        val rotated = angle.rotateInDegree(rotate)
        pod.copy(angle = rotated, mass = 10.0)
      case Thrust(_, target, thrust) =>
        val pivoted = pod.angle.pivotTo(target - pod.position, 18.0)
        val realThrust = if (state.context.shieldCoolDown(action.id) > 0) 0 else thrust
        pod.copy(speed = pod.speed + pivoted * realThrust, angle = pivoted)
      case AngleThrust(_, _, angle, rotate, thrust) =>
        val rotated = angle.rotateInDegree(rotate)
        val realThrust = if (state.context.shieldCoolDown(action.id) > 0) 0 else thrust
        pod.copy(speed = pod.speed + rotated * realThrust, angle = rotated)
      case _ => pod
    }

    val movedPods = StrikeBackCollisionSimulation.simulate(state.context.checkPoints, steeredPods, 1.0)

    val slowed = movedPods.map(p => p.copy(speed = p.speed * .85))

    StrikeBackState(StrikeBackController.nextContext(state.context, state, actions), slowed)
  }

  override def judge(state: StrikeBackState): GameResult = {
    Draw
  }

}