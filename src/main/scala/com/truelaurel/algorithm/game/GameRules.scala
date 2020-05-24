package com.truelaurel.algorithm.game

import scala.annotation.tailrec
import scala.util.Random

trait GameRules[P, S <: GameState[P], M] {
  def initial: S

  def validMoves(state: S): Seq[M]

  def applyMove(state: S, move: M): S

  def outcome(state: S): Outcome[P]

  @tailrec
  final def judge(
      players: Map[P, S => M],
      debug: S => Unit,
      state: S = initial
  ): Outcome[P] = {
    debug(state)
    outcome(state) match {
      case Undecided =>
        val p = players(state.nextPlayer)
        val m = p(state)
        judge(players, debug, applyMove(state, m))
      case o => o
    }
  }

  def randomMove(s: S): M = {
    val moves = validMoves(s)
    require(moves.nonEmpty, "no valid moves in state " + s)
    moves(Random.nextInt(moves.size))
  }

  def randomPlay(state: S): Outcome[P] =
    playUntilEnd(randomMove)(state)

  def playUntilEnd(selectMove: S => M)(state: S): Outcome[P] = {
    @tailrec
    def playRec(s: S): Outcome[P] = {
      outcome(s) match {
        case Undecided => playRec(applyMove(s, selectMove(s)))
        case decided   => decided
      }
    }

    playRec(state)
  }
}
