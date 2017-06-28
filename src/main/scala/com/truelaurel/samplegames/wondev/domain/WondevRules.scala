package com.truelaurel.samplegames.wondev.domain

import com.truelaurel.algorithm.game._

case class WondevRules(size: Int) extends RulesFor2p[FastState, WondevAction] {
  def initial: FastState =
    FastState(size, myUnits = Array(0, 1), opUnits = Array(3, 2))

  def validMoves(state: FastState): Seq[WondevAction] =
    state.validActions

  def applyMove(state: FastState, move: WondevAction): FastState =
    state.applyAction(move)

  def outcome(state: FastState): Outcome[Boolean] = {
    val iLost = state.myInactive && state.myScore < state.opScore
    val iWon = state.opInactive && state.myScore > state.opScore
    val draw = state.opInactive && state.myInactive && state.myScore == state.opScore

    if (iLost)
      Wins(false)
    else if (iWon)
      Wins(true)
    else if (draw)
      Draw
    else Undecided
  }
}
