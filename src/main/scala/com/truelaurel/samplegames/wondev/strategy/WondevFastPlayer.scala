package com.truelaurel.samplegames.wondev.strategy

import com.truelaurel.algorithm.mcts.MctsAi
import com.truelaurel.codingame.challenge.GamePlayer
import com.truelaurel.codingame.logging.CGLogger
import com.truelaurel.math.geometry.Pos
import com.truelaurel.samplegames.wondev.analysis.WondevAnalysis
import com.truelaurel.samplegames.wondev.arena.WondevArena
import com.truelaurel.samplegames.wondev.domain._
import com.truelaurel.time.Chronometer

import scala.concurrent.duration.DurationInt


case class WondevFastPlayer(side: Boolean, size: Int) extends GamePlayer[FastState, WondevAction] {
  val rules = WondevRules(size)

  def mctsMove(s: FastState): WondevAction = {
    val chronometer = new Chronometer(40.millis)
    chronometer.start()
    val (move, count) = MctsAi(rules)(_ => chronometer.willOutOfTime).chooseMoveCount(s)
    CGLogger.info(s"$count nodes explored")
    move
  }

  override def reactTo(state: FastState): Vector[WondevAction] =
    if (state.validActions.isEmpty) Vector(Pass)
    else {
      Vector(mctsMove(state))
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
