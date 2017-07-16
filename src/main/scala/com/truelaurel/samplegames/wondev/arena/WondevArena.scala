package com.truelaurel.samplegames.wondev.arena

import com.truelaurel.math.geometry.Pos
import com.truelaurel.samplegames.wondev.analysis.WondevAnalysis
import com.truelaurel.samplegames.wondev.domain._

/**
  * Created by hwang on 24/06/2017.
  */
object WondevArena {
  def next(fromState: WondevState, action: WondevAction): WondevState = action match {
    case MoveBuild(unitIndex, move, build) =>
      fromState.copy(
        units = fromState.units.updated(unitIndex, move),
        heightMap = fromState.heightMap.updated(build, fromState.heightOf(build) + 1),
        nextPlayer = !fromState.nextPlayer
      )

    case PushBuild(unitIndex, build, push) =>
      val pushedUnitIndex = fromState.units.indexOf(build)
      fromState.copy(
        units = fromState.units.updated(pushedUnitIndex, push),
        heightMap = fromState.heightMap.updated(build, fromState.heightOf(build) + 1),
        nextPlayer = !fromState.nextPlayer
      )
    case AcceptDefeat =>
      throw new IllegalStateException("Unable to simulate this defeat action")
  }

  def nextLegalActions(state: WondevState): Seq[WondevAction] = {
    val myStart = if (state.nextPlayer) 0 else 2
    val opStart = if (state.nextPlayer) 2 else 0
    val units = state.units.slice(myStart, myStart + 2)
    val opUnits = state.units.slice(opStart, opStart + 2)


    val buildActions = for {
      id <- myStart until myStart + 2
      unit = units(id)
      h = state.heightOf(unit)
      target1 <- state.neighborOf(unit)
      h1 = state.heightOf(target1)
      if WondevContext.isPlayable(h1) && h + 1 >= h1 && state.isFree(target1)
      target2 <- state.neighborOf(target1)
      h2 = state.heightOf(target2)
      if WondevContext.isPlayable(h2) && state.isFree(target2)
    } yield MoveBuild(id, target1, target2)

    val pushActions = for {
      id <- myStart until myStart + 2
      unit = units(id)
      opId <- opStart until opStart + 2
      target1 = units(opId)
      h1 = state.heightOf(target1)
      if WondevAnalysis.isVisible(target1) && unit.distance(target1) == 1
      pushTargets: Array[Pos] = WondevContext.pushTargets(state.size)(unit, target1)
      target2 <- pushTargets
      h2 = state.heightOf(target2)
      if WondevContext.isPlayable(h2) && h1 + 1 >= h2 && state.isFree(target2)
    } yield PushBuild(id, target1, target2)

    buildActions ++ pushActions
  }

}
