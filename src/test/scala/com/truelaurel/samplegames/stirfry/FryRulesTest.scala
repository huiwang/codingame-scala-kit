package com.truelaurel.samplegames.stirfry

import org.scalatest.{FlatSpec, Matchers}

class FryRulesTest extends FlatSpec with Matchers {

  "Fry rules" should "handle discard 2" in {
    val board = FryBoard(
      hands = Seq(CardStack(List(Noodles, Noodles))),
      drawStack = CardStack(List(Pork, Chicken, Shrimp)))

    val nextBoard = FryRules.applyMove(board, DiscardPair(Noodles, Noodles))
    nextBoard.drawStack.empty shouldBe true
    nextBoard.hands.head.cards should contain theSameElementsAs Seq(Pork, Chicken, Shrimp)
  }

  it should "handle discard pork" in {
    val board = FryBoard(
      hands = Seq(CardStack(List(Soja, Noodles))),
      drawStack = CardStack(List(Ginger, Chicken, Shrimp)))

    val nextBoard = FryRules.applyMove(board, DiscardMeat(Soja, Pork))
    nextBoard.drawStack.empty shouldBe true
    nextBoard.hands.head.cards should contain theSameElementsAs Seq(Ginger, Noodles, Chicken, Shrimp)
  }

  it should "score and draw for cook" in {
    val board = FryBoard(hands = Seq(CardStack(List(Soja, Noodles, Chicken)), CardStack()), drawStack = CardStack(List(Pork)))

    val nextBoard = FryRules.applyMove(board, Cook(Set(Soja, Noodles, Chicken)))
    nextBoard.hands.head.cards should contain theSameElementsAs Seq(Pork)
    nextBoard.scores.head shouldBe 4
    nextBoard.nextPlayer shouldBe 1
  }

  it should "shuffle at end of turn" in {
    val board = FryBoard(
      hands = Seq(CardStack()),
      drawStack = CardStack(List(Pork)),
      discardStack = CardStack(List(Chicken)),
      scores = Seq(0)
    )

    val nextBoard = FryRules.applyMove(board, Pass)
    nextBoard.drawStack.cards should contain theSameElementsAs List(Pork, Chicken)
    nextBoard.discardStack.empty shouldBe true
  }
}
