package com.truelaurel.samplegames.wondev.strategy

import com.truelaurel.algorithm.game.{Outcome, RulesFor2p, Undecided}
import com.truelaurel.samplegames.wondev.analysis.WondevAnalysis
import com.truelaurel.samplegames.wondev.arena.UndoWondevArena
import com.truelaurel.samplegames.wondev.domain._

/**
  * Created by hwang on 28/06/2017.
  */
class WondevMinimax(val initial: WondevState) extends RulesFor2p[WondevState, WondevAction] {

  var i = 0

  override def validMoves(state: WondevState): Seq[WondevAction] = {
    i += 1
    if (i == 1) state.legalActions else ???
  }

  override def applyMove(state: WondevState, move: WondevAction): WondevState = ???

  override def outcome(state: WondevState): Outcome[Boolean] = Undecided
}

object MinimaxPlayer {
  def react(state: WondevState): WondevAction = {
    /*    val cleaned = WondevAnalysis.removeFog(state)
        val rules = new WondevMinimax(cleaned)
        val minimax = AlphaBetaAi2(rules, WondevAnalysis.evaluate, moveScore)
        val action = minimax.bestMove(rules.initial, depth = 2)*/
    if (state.legalActions.isEmpty) AcceptDefeat else {
      val fastWondevState = FastWondevState.fromSlowState(state)
      state.legalActions.maxBy(action => {
        val simulated = UndoWondevArena.next(fastWondevState, action)
        val score = WondevAnalysis.evaluate(simulated)
        simulated.undoable.undo()
        score
      })
    }
  }

  def moveScore(action: WondevAction, state: WondevState): Double = {
    0.0
  }

}