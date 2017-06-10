package ai.scala.fp.game.mcts

import ai.scala.fp.game._
import org.scalatest.{FlatSpec, Matchers}

class MctsNodeTest extends FlatSpec with Matchers {

  behavior of "MctsNode"

  case class DummyState(s: Seq[Int], nextPlayer: Boolean) extends GameState[Boolean]

  val dummyRules = new RulesFor2p[DummyState, Int] {
    def initial = DummyState(Seq(1, 2, 3, 4), true)

    def validMoves(state: DummyState): Seq[Int] = state.s

    def applyMove(state: DummyState, move: Int): DummyState =
      DummyState(state.s.diff(Seq(move)), !state.nextPlayer)

    def outcome(state: DummyState): Outcome[Boolean] = state.s match {
      case Seq(1) => if (state.nextPlayer) Wins(false) else Wins(true)
      case _ => Undecided
    }

  }
  val rootNode = MctsNode(dummyRules.initial, dummyRules, randomPlay)

  def randomPlay(state: DummyState): Outcome[Boolean] = dummyRules.outcome(state)


  it should "select least played node" in {
    val child1 = MctsNode(DummyState(Seq(2), false), dummyRules, randomPlay, Results(1, 0))
    val root = MctsNode(DummyState(Seq(1, 2), true), dummyRules, randomPlay, Results(1, 0), Map(1 -> child1))

    root.moveToExplore shouldBe 2
  }
}
