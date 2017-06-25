package com.truelaurel.samplegames.wondev.strategy

import com.truelaurel.codingame.challenge.GamePlayer
import com.truelaurel.samplegames.wondev.analysis.WondevAnalysis
import com.truelaurel.samplegames.wondev.arena.WondevArena
import com.truelaurel.samplegames.wondev.domain._

case class WondevPlayer(side: Boolean) extends GamePlayer[WondevState, WondevAction] {

  override def reactTo(state: WondevState): Vector[WondevAction] = {
    val opPos = WondevAnalysis
      .findDelta(state)
      .flatMap(pos => {
        val oppoNeighbors = state.neighborMap(pos).filter(p => {
          val h = state.heightMap(p)
          h != -1 && h != 4
        })
        val seenNeighbors = state.myUnits.flatMap(unit => state.neighborMap(unit) + unit)
        val feasibleNeighbors = oppoNeighbors -- seenNeighbors
        if (feasibleNeighbors.isEmpty) None else Some(feasibleNeighbors.maxBy(state.heightMap))
      })
    val myAction = state.legalActions.
      map(action => {
        action.actionType match {
          case Build => MoveBuild(action.unitIndex, action.dir1, action.dir2)
          case Push => MovePush(action.unitIndex, action.dir1, action.dir2)
        }
      }).maxBy(action => {
      val nextState = WondevArena.next(state, Vector(action))
      WondevAnalysis.evaluate(nextState, opPos)
    })
    Vector(myAction)
  }
}
