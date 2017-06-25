package com.truelaurel.samplegames.wondev.arena

import com.truelaurel.codingame.challenge.{GameArena, GameResult}
import com.truelaurel.samplegames.wondev.analysis.WondevAnalysis
import com.truelaurel.samplegames.wondev.domain.{MoveBuild, MovePush, WondevAction, WondevState}

/**
  * Created by hwang on 24/06/2017.
  */
object WondevArena extends GameArena[WondevState, WondevAction] {
  override def next(fromState: WondevState, actions: Vector[WondevAction]): WondevState = {
    val action = actions.head
    action match {
      case MoveBuild(unitIndex, moveDir, buildDir) =>
        val unit = fromState.myUnits(unitIndex)
        val moveTarget = unit.neighborIn(moveDir)
        val buildTarget = moveTarget.neighborIn(buildDir)

        fromState.copy(
          turn = fromState.turn + 1,
          myUnits = fromState.myUnits.updated(unitIndex, moveTarget),
          heights = fromState.heights.updated(buildTarget, fromState.heights(buildTarget) + 1))

      case MovePush(unitIndex, moveDir, pushDir) =>
        val unit = fromState.myUnits(unitIndex)
        val moveTarget = unit.neighborIn(moveDir)
        val pushTarget = moveTarget.neighborIn(pushDir)
        val pushedUnitIndex = fromState.opUnits.indexOf(moveTarget)
        fromState.copy(
          turn = fromState.turn + 1,
          heights = fromState.heights.updated(moveTarget, fromState.heights(moveTarget) + 1),
          opUnits = fromState.opUnits.updated(pushedUnitIndex, pushTarget)
        )
    }


  }

  override def judge(state: WondevState): GameResult = ???
}
