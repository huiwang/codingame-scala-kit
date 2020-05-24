package com.truelaurel.algorithm.game

trait RulesFor2p[S <: GameState[Boolean], M] extends GameRules[Boolean, S, M] {

  def judge(
      truePl: S => M,
      falsePl: S => M,
      debug: S => Unit
  ): Outcome[Boolean] =
    judge(Map(true -> truePl, false -> falsePl), debug, initial)

}
