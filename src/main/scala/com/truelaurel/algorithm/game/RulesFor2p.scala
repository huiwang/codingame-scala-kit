package com.truelaurel.algorithm.game

import scala.annotation.tailrec

trait RulesFor2p[S <: GameState[Boolean], M]
  extends GameRules[Boolean, S, M] {


  @tailrec
  final def judge(truePl: S => M,
                  falsePl: S => M,
                  debug: S => Unit,
                  state: S = initial): Outcome[Boolean] = {
    debug(state)
    outcome(state) match {
      case Undecided =>
        val m = if (state.nextPlayer) truePl(state) else falsePl(state)
        judge(truePl, falsePl, debug, applyMove(state, m))
      case o => o
    }
  }
}
