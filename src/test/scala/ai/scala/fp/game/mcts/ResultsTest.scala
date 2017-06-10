package ai.scala.fp.game.mcts

import ai.scala.fp.game.Wins
import org.scalatest.{FlatSpec, Matchers}

class ResultsTest extends FlatSpec with Matchers {

  behavior of "Results"

  it should "keep wins" in {
    val res = Results()
    res.played shouldBe 0
    res.score shouldBe 0

    val p1 = res.withOutcome(Wins(true))
    p1.played shouldBe 1
    p1.score shouldBe 1
    p1.wins(true) shouldBe 1
    p1.wins(false) shouldBe 0
  }

  it should "keep losses" in {
    val res = Results()

    val p1 = res.withOutcome(Wins(false))
    p1.played shouldBe 1
    p1.score shouldBe -1
    p1.wins(true) shouldBe 0
    p1.wins(false) shouldBe 1
  }

  it should "find best move with current results" in {
    val moves = Map(
      1 -> Results(played = 3, score = 2),
      2 -> Results(played = 3, score = 1))

    Results.mostPromisingMove(true, moves) shouldBe 1
    Results.mostPromisingMove(false, moves) shouldBe 2
  }

  it should "find move with no results" in {
    val moves = Map(
      1 -> Results(played = 3, score = 2),
      3 -> Results(played = 0, score = 0),
      2 -> Results(played = 3, score = 1))

    Results.mostPromisingMove(true, moves) shouldBe 3
    Results.mostPromisingMove(false, moves) shouldBe 3
  }

}
