package com.truelaurel.samplegames.wondev.strategy

import com.truelaurel.codingame.challenge.GamePlayer
import com.truelaurel.codingame.logging.CGLogger
import com.truelaurel.math.geometry.Pos
import com.truelaurel.samplegames.wondev.analysis.WondevAnalysis
import com.truelaurel.samplegames.wondev.arena.WondevArena
import com.truelaurel.samplegames.wondev.domain._

case class WondevPlayer(side: Boolean) extends GamePlayer[WondevState, WondevAction] {

  override def reactTo(state: WondevState): Vector[WondevAction] =
    if (state.legalActions.isEmpty) Vector(Pass)
    else {
      val opPos = guessOppPos(state)
      val scoredActions = actions(state).map(a => a -> eval(state, opPos, a)).sortBy(-_._2)
      scoredActions.foreach(CGLogger.debug _)
      val myAction = scoredActions.maxBy(_._2)._1
      Vector(myAction)
    }

  private def eval(state: WondevState, opPos: Option[Pos], action: WondevAction) = {
    val nextState = WondevArena.next(state, Vector(action))
    WondevAnalysis.evaluate(nextState, opPos)
  }

  private def actions(state: WondevState) = {
    state.legalActions.
      map(action => {
        action.actionType match {
          case Build => MoveBuild(action.unitIndex, action.dir1, action.dir2)
          case Push => MovePush(action.unitIndex, action.dir1, action.dir2)
        }
      })
  }

  private def guessOppPos(state: WondevState) = {
    WondevAnalysis
      .findDelta(state)
      .flatMap(pos => {
        val oppoNeighbors = state.neighborMap(pos).filter(p => {
          val h = state.heightMap(p)
          h != -1 && h != 4
        }).toSet
        val seenNeighbors = state.myUnits.flatMap(unit => state.neighborMap(unit).toSet + unit)
        val feasibleNeighbors = oppoNeighbors -- seenNeighbors
        if (feasibleNeighbors.isEmpty) None else Some(feasibleNeighbors.maxBy(state.heightMap))
      })
  }
}
