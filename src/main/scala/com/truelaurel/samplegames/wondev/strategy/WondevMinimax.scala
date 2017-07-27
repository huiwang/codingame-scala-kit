package com.truelaurel.samplegames.wondev.strategy

import com.truelaurel.algorithm.alphabeta.AlphaBetaAi2
import com.truelaurel.algorithm.game.{Outcome, RulesFor2p, Undecided}
import com.truelaurel.samplegames.wondev.analysis.WondevEvaluator
import com.truelaurel.samplegames.wondev.domain._
import com.truelaurel.samplegames.wondev.simulation.WondevSimulator

/**
  * Created by hwang on 28/06/2017.
  */
class WondevMinimax(val initial: FastWondevState) extends RulesFor2p[FastWondevState, WondevAction] {

  override def validMoves(state: FastWondevState): Seq[WondevAction] = {
    WondevSimulator.nextLegalActions(state)
  }

  override def applyMove(state: FastWondevState, move: WondevAction): FastWondevState = WondevSimulator.next(state, move)

  override def outcome(state: FastWondevState): Outcome[Boolean] = Undecided
}

object MinimaxPlayer {
  def react(state: WondevState): WondevAction = {

    if (state.legalActions.isEmpty) AcceptDefeat else {
      val fastWondevState = FastWondevState.fromSlowState(state)
      val rules = new WondevMinimax(fastWondevState)
      val minimax = AlphaBetaAi2(rules, WondevEvaluator.evaluate, moveScore)
      val action = minimax.bestMove(rules.initial, depth = 2)
      action
    }
  }

  def moveScore(action: WondevAction, state: FastWondevState): Double = {
    -1.0 * (action match {
      case MoveBuild(unitIndex, move, build) => state.readable.heightOf(build)
      case PushBuild(unitIndex, build, push) => 10 + state.readable.heightOf(build)
      case AcceptDefeat => 0.0
    })
  }

}