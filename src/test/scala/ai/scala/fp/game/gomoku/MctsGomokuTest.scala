package ai.scala.fp.game.gomoku

import ai.scala.fp.game.mcts.MctsAi
import ai.scala.fp.geo.Pos
import org.scalatest.{FlatSpec, Matchers}

class MctsGomokuTest extends FlatSpec with Matchers {

  behavior of "MctsAi for Gomoku"

  val rules = GomokuRules(3, 3)
  val ai = MctsAi(rules)(_.results.played > 50)


  it should "find the best move for false when it's a win" in {
    val root = GomokuBoard(3)
    val falseToWin = root.play(2, 0).play(0, 0).play(2, 2).play(1, 1)
    println(falseToWin.toText)
    ai.chooseMove(falseToWin) shouldBe Pos(2, 1)
  }

  it should "find the best move for true when it's a win" in {
    val root = GomokuBoard(3)
    // F T
    // FTF
    // X
    val trueToWin = root.play(0, 0).play(0, 2).play(1, 0).play(1, 1).play(2, 1)
    println(trueToWin.toText)
    ai.chooseMove(trueToWin) shouldBe Pos(2, 0)
  }

  it should "find the best move for false to avoid sure defeat" in {
    val root = GomokuBoard(3)
    //  FT
    //
    // T F
    val trueToWin = root.play(1, 0).play(2, 0).play(2, 2).play(0, 2)
    println(trueToWin.toText)
    ai.chooseMove(trueToWin) shouldBe Pos(1, 1)
  }

}
