package ai.scala.fp.game.alphabeta

import ai.scala.fp.game._

case class AlphaBetaAi[S <: GameState[Boolean], M](rules: RulesFor2p[S, M],
                                                   heuristic: S => Double) {

  def chooseMove(state: S, depth: Int): M = {
    val moves = rules.validMoves(state)
    //TODO : can we cut branches also from root node ??
    moves.minBy(m => negamax(rules.applyMove(state, m), depth - 1))
  }

  def negamax(state: S,
              depth: Int,
              alphaIni: Double = Double.MinValue,
              betaIni: Double = Double.MaxValue): Double = {
    val player = state.nextPlayer
    rules.outcome(state) match {
      case Wins(`player`) => Double.MaxValue
      case Wins(_) => Double.MinValue
      case Draw => 0
      case Undecided =>
        val moves = rules.validMoves(state)
        if (depth == 0 || moves.isEmpty) heuristic(state)
        else {
          val (_, best) = moves.foldLeft((alphaIni, Double.MinValue)) {
            case ((alpha, beta), move) =>
              if (betaIni > alpha) {
                val nextState = rules.applyMove(state, move)
                val evaluation = -negamax(nextState, depth - 1, -betaIni, -alpha)
                (alpha max evaluation, beta max evaluation)
              } else {
                (alpha, beta)
              }
          }
          println(s"$best is the score for $state")
          best
        }
    }
  }
}
