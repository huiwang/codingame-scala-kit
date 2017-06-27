package com.truelaurel.samplegames.wondev.domain

import com.truelaurel.algorithm.game.{Outcome, RulesFor2p, Undecided, Wins}

case class WondevRules(size: Int) extends RulesFor2p[FastState, WondevAction] {
  def initial: FastState =
    FastState(size, myUnits = Vector(0, 1), opUnits = Vector(1, 2))

  def validMoves(state: FastState): Seq[WondevAction] =
    state.validActions

  def applyMove(state: FastState, move: WondevAction): FastState =
    state.applyAction(move)

  def outcome(state: FastState): Outcome[Boolean] =
    if (state.validActions.head == Pass) {
      if (state.scoreForNext < state.scoreForOther) Wins(!state.nextPlayer)
      else Undecided
    } else Undecided
  //TODO : draws
}
