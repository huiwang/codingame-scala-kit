package com.truelaurel.samplegames.dummy

import com.truelaurel.algorithm.game._

case class DummyRules(branching: Int = 100, depth: Int = 20) extends RulesFor2p[DummyBoard, Int] {
  val initial: DummyBoard = DummyBoard(Nil, false)

  val moves = 1 to branching

  def validMoves(state: DummyBoard): Seq[Int] = moves

  def applyMove(state: DummyBoard, move: Int): DummyBoard =
    DummyBoard(move :: state.played, !state.nextPlayer)

  def outcome(state: DummyBoard): Outcome[Boolean] =
    if (state.played.size < depth) Undecided
    else if (state.played.head % 2 == 0) Wins(true)
    else Wins(false)
}

case class DummyBoard(played: List[Int], nextPlayer: Boolean)
  extends GameState[Boolean]