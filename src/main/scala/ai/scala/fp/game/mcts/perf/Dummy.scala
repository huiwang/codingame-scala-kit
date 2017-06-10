package ai.scala.fp.game.mcts.perf

import ai.scala.fp.game._
import ai.scala.fp.game.mcts.MctsNode

object Dummy {
  val dummyRoot = MctsNode(DummyRules.initial, DummyRules, DummyRules.randomPlay)

  case class DummyBoard(played: List[Int], nextPlayer: Boolean)
    extends GameState[Boolean]

  type DummyMove = Int

  object DummyRules extends RulesFor2p[DummyBoard, DummyMove] {
    def initial: DummyBoard = DummyBoard(Nil, false)

    val moves = 1 to 100

    def validMoves(state: DummyBoard): Seq[DummyMove] = moves

    def applyMove(state: DummyBoard, move: DummyMove): DummyBoard =
      DummyBoard(move :: state.played, !state.nextPlayer)

    def outcome(state: DummyBoard): Outcome[Boolean] =
      if (state.played.size < 20) Undecided
      else if (state.played.head % 2 == 0) Wins(true)
      else Wins(false)
  }


}
