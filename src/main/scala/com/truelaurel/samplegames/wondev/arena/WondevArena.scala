package com.truelaurel.samplegames.wondev.arena

import com.truelaurel.codingame.challenge.{GameArena, GameResult}
import com.truelaurel.math.geometry.Direction
import com.truelaurel.samplegames.wondev.analysis.WondevAnalysis
import com.truelaurel.samplegames.wondev.domain._

/**
  * Created by hwang on 24/06/2017.
  */
object WondevArena extends GameArena[WondevState, WondevAction] {
  override def next(fromState: WondevState, actions: Vector[WondevAction]): WondevState = {
    actions.head match {
      case MoveBuild(unitIndex, moveDir, buildDir) =>
        handleMoveBuild(fromState, unitIndex, moveDir, buildDir)

      case MovePush(unitIndex, moveDir, pushDir) =>
        handleMovePush(fromState, unitIndex, moveDir, pushDir)

      case Pass => fromState
    }
  }

  private def handleMovePush(fromState: WondevState, unitIndex: Int, moveDir: Direction, pushDir: Direction) = {
    val unit = fromState.myUnits(unitIndex)
    val moveTarget = unit.neighborIn(moveDir)
    val pushTarget = moveTarget.neighborIn(pushDir)
    val pushedUnitIndex = fromState.opUnits.indexOf(moveTarget)
    fromState.copy(
      turn = fromState.turn + 1,
      heightMap = fromState.heightMap.updated(moveTarget, fromState.heightMap(moveTarget) + 1),
      opUnits = fromState.opUnits.updated(pushedUnitIndex, pushTarget)
    )
  }

  private def handleMoveBuild(fromState: WondevState, unitIndex: Int, moveDir: Direction, buildDir: Direction) = {
    val unit = fromState.myUnits(unitIndex)
    val moveTarget = unit.neighborIn(moveDir)
    val buildTarget = moveTarget.neighborIn(buildDir)

    fromState.copy(
      turn = fromState.turn + 1,
      myUnits = fromState.myUnits.updated(unitIndex, moveTarget),
      heightMap = fromState.heightMap.updated(buildTarget, fromState.heightMap(buildTarget) + 1))
  }

  override def judge(state: WondevState): GameResult = ???
}
