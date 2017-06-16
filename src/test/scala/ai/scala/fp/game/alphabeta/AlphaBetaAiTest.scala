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
    case _ => 0
  }

  it should "select best move" in {
    val chosenMove = AlphaBetaAi(dummyRules, heuristic).chooseMove(dummyRules.initial, 3)
    chosenMove shouldBe "b"
  }

  case class IntsState(nextPlayer: Boolean = true, trueNumbers: List[Int] = Nil, falseNumbers: List[Int] = Nil)
    extends GameState[Boolean] {

    def play(i: Int): IntsState = if (nextPlayer)
      copy(false, trueNumbers = i :: trueNumbers)
    else copy(true, falseNumbers = i :: falseNumbers)

  }

  val dummyUndecidedRules = new RulesFor2p[IntsState, Int] {
    def initial = IntsState()

    def validMoves(state: IntsState): Seq[Int] =
      Seq(3, 5, 7, 1, 9, 2, 10, 4, 6, 8)


    def applyMove(state: IntsState, move: Int): IntsState =
      state.play(move)

    def outcome(state: IntsState): Outcome[Boolean] = Undecided
  }

  def heur(state: IntsState) =
    if (state.nextPlayer)
      state.trueNumbers.sum - state.falseNumbers.sum
    else
      state.falseNumbers.sum - state.trueNumbers.sum

  it should "select move with higher heuristic and depth 1" in {
    val chosenMove = AlphaBetaAi(dummyUndecidedRules, heur).chooseMove(dummyUndecidedRules.initial, 1)
    chosenMove shouldBe 10
  }

  it should "select move with higher heuristic and depth 2" in {
    val chosenMove = AlphaBetaAi(dummyUndecidedRules, heur).chooseMove(dummyUndecidedRules.initial, 2)
    chosenMove shouldBe 10
  }

  it should "select move with higher heuristic and depth 3" in {
    val chosenMove = AlphaBetaAi(dummyUndecidedRules, heur).chooseMove(dummyUndecidedRules.initial, 3)
    chosenMove shouldBe 10
  }

}
