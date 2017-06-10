package ai.scala.fp.game.alphabeta

import ai.scala.fp.game._
import org.scalatest.{FlatSpec, Matchers}

class AlphaBetaAiTest extends FlatSpec with Matchers {

  behavior of "AlphaBetaAi"

  case class DummyState(name: String, nextPlayer: Boolean)
    extends GameState[Boolean]

  val dummyRules = new RulesFor2p[DummyState, String] {
    def initial = DummyState("a", true)

    def validMoves(state: DummyState): Seq[String] = state.name match {
      case "a" => Seq("b", "f")
      case "b" => Seq("c", "d")
      case "f" => Seq("e", "g")
      case "g" => Seq("z")
      case "d" => Seq("x", "y")
      case _ => Nil
    }

    //         a
    //        / \
    //      b    f
    //     / \  / \
    //    c  d  e  g
    //     / \      \
    //    x  y       z

    def applyMove(state: DummyState, move: String): DummyState =
      DummyState(move, !state.nextPlayer)

    def outcome(state: DummyState): Outcome[Boolean] = state.name match {
      case "y" => Wins(false)
      case "e" => Wins(true)
      case "c" => Draw
      case _ => Undecided
    }
  }

  def heuristic(state: DummyState): Double = state.name match {
    case "x" => 2
    case "z" => 4
  }

  it should "select best move" in {
    val chosenMove = AlphaBetaAi(dummyRules, heuristic).chooseMove(dummyRules.initial, 3)
    chosenMove shouldBe "b"
  }
}
