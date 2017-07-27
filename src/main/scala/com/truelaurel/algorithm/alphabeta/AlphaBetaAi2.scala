package com.truelaurel.algorithm.alphabeta

import com.truelaurel.algorithm.game._
import com.truelaurel.samplegames.wondev.domain.{FastWondevState, WondevAction}

/**
  * @param heuristic must represent higher chance of success for state.nextPlayer
  */
case class AlphaBetaAi2(rules: RulesFor2p[FastWondevState, WondevAction],
                        heuristic: FastWondevState => Double,
                        moveHeuristc: (WondevAction, FastWondevState) => Double
                       ) {

  type S = FastWondevState
  type M = WondevAction

  val MIN = Double.MinValue
  val MAX = Double.MaxValue


  def bestMove(state: S, depth: Int): M = {
    val moves = rules.validMoves(state).sortBy(moveHeuristc(_, state))

    var value = MIN
    var i = 0
    var currentAlpha = MIN
    var bestMove = null.asInstanceOf[M]
    while (i < moves.size) {
      val move = moves(i)

      val valueForMove = alphabeta(rules.applyMove(state, move), depth - 1, currentAlpha, MAX, maximizingPlayer = false)
      state.writable.undo()
      if (valueForMove > value) {
        bestMove = move
        value = valueForMove
        currentAlpha = valueForMove
      }
      i += 1
    }
    bestMove
  }

  def alphabeta(state: S, depth: Int, alpha: Double, beta: Double, maximizingPlayer: Boolean): Double = {
    if (depth == 0) {
      heuristic(state)
    } else {
      val moves = rules.validMoves(state).sortBy(m => moveHeuristc(m, state))
      if (moves.isEmpty) {
        heuristic(state)
      } else {
        if (maximizingPlayer) {
          var value = MIN
          var i = 0
          var currentAlpha = alpha
          while (i < moves.size) {
            val move = moves(i)
            value = Math.max(value, alphabeta(rules.applyMove(state, move), depth - 1, currentAlpha, beta, maximizingPlayer = false))
            state.writable.undo()
            currentAlpha = Math.max(currentAlpha, value)
            if (beta <= currentAlpha)
              return value
            i += 1
          }
          value
        } else {
          var value = MAX
          var i = 0
          var currentBeta = beta
          while (i < moves.size) {
            val move = moves(i)
            value = Math.min(value, alphabeta(rules.applyMove(state, move), depth - 1, alpha, currentBeta, maximizingPlayer = true))
            state.writable.undo()
            currentBeta = Math.min(currentBeta, value)
            if (currentBeta <= alpha)
              return value
            i += 1
          }
          value
        }
      }
    }
  }

  case class ScoredMove(score: Double, move: Option[M])

}
