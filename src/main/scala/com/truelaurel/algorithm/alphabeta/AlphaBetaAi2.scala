package com.truelaurel.algorithm.alphabeta

import com.truelaurel.algorithm.game._

import scala.annotation.tailrec

/**
  * @param heuristic must represent higher chance of success for state.nextPlayer
  */
case class AlphaBetaAi2[S <: GameState[Boolean], M](rules: RulesFor2p[S, M],
                                                    heuristic: S => Double,
                                                    moveHeuristc: (M, S) => Double
                                                  ) {

  val MIN = Double.MinValue
  val MAX = Double.MaxValue

  def chooseMove(state: S, depth: Int): M = {
    val sorted = sortedMoves(state)
    best(sorted, MIN, MAX, state, depth).move.getOrElse(sorted.head)
  }


  def sortedMoves(state: S): Seq[M] = {
    rules.validMoves(state)
  }

  def negamax(state: S,
              depth: Int,
              alpha: Double,
              beta: Double): Double = {
    val player = state.nextPlayer
    val color = if (player) 1.0 else -1.0
    rules.outcome(state) match {
      case Wins(`player`) => MAX
      case Wins(_) => MIN
      case Draw => 0
      case Undecided =>
        if (depth == 0) {
          color * heuristic(state)
        } else {
          val moves = sortedMoves(state)
          if (moves.isEmpty) {
            color * heuristic(state)
          } else {
            best(moves, alpha, beta, state, depth).score
          }
        }
    }
  }


  def bestMove(state: S, depth: Int): M = {
    val moves = rules.validMoves(state).sortBy(moveHeuristc(_, state))

    var value = MIN
    var i = 0
    var currentAlpha = MIN
    var bestMove = null.asInstanceOf[M]
    while (i < moves.size) {
      val move = moves(i)

      val valueForMove = alphabeta(rules.applyMove(state, move), depth - 1, currentAlpha, MAX, maximizingPlayer = false)
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


  @tailrec
  final def best(moves: Seq[M],
                 alpha: Double,
                 beta: Double,
                 state: S,
                 depth: Int,
                 currentBest: Option[M] = None): ScoredMove = {
    if (beta > alpha && moves.nonEmpty) {
      val move = moves.head
      val nextState = rules.applyMove(state, move)
      val evaluation = -negamax(nextState, depth - 1, -beta, -alpha)
      val newBest = if (evaluation > alpha) Some(move) else currentBest
      best(moves.tail, alpha max evaluation, beta max evaluation, state, depth, newBest)
    } else {
      ScoredMove(alpha, currentBest)
    }
  }


  case class ScoredMove(score: Double, move: Option[M])

}
