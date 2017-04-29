package com.truelaurel.codingame.csb.arena

import com.truelaurel.codingame.csb.model._
import com.truelaurel.codingame.engine.{Draw, GameArena, GameResult}

/**
  * Created by hwang on 02/04/2017.
  */
object StrikeBackArena extends GameArena[StrikeBackState, StrikeBackAction] {

  override def next(state: StrikeBackState, actions: Vector[StrikeBackAction]): StrikeBackState = {
    val steeredPods = for {
      action <- actions
      pod = state.pods(action.id)
    } yield action match {
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