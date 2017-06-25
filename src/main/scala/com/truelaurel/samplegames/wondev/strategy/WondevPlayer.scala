package com.truelaurel.samplegames.wondev.strategy

import com.truelaurel.codingame.challenge.GamePlayer
import com.truelaurel.samplegames.wondev.analysis.WondevAnalysis
import com.truelaurel.samplegames.wondev.arena.WondevArena
import com.truelaurel.samplegames.wondev.domain._

case class WondevPlayer(side: Boolean) extends GamePlayer[WondevState, WondevAction] {
  override def reactTo(state: WondevState): Vector[WondevAction] = {
    val myAction = state.legalActions.
      map(action => {
        action.actionType match {
          case Build => MoveBuild(action.unitIndex, action.dir1, action.dir2)
          case Push => MovePush(action.unitIndex, action.dir1, action.dir2)
        }
      }).maxBy(action => {
      val nextState = WondevArena.next(state, Vector(action))
      WondevAnalysis.evaluate(nextState)
    })
    Vector(myAction)
  }
}
