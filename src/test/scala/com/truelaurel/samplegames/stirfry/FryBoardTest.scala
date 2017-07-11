package com.truelaurel.samplegames.stirfry

import org.scalatest.{FlatSpec, Matchers}

class FryBoardTest extends FlatSpec with Matchers {

  "a board" should "handle discard 2" in {
    val board = FryBoard(
      hands = Seq(CardStack(List(Noodles, Noodles))),
      drawStack = CardStack(List(Pork, Chicken, Shrimp)))

    val nextBoard = board.applyMove(DiscardPair(Noodles, Noodles))
    nextBoard.drawStack.empty shouldBe true
    nextBoard.hands.head.cards should contain theSameElementsAs Seq(Pork, Chicken, Shrimp)
  }

  it should "handle discard pork" in {
    val board = FryBoard(
      hands = Seq(CardStack(List(Soja, Noodles))),
      drawStack = CardStack(List(Ginger, Chicken, Shrimp)))

    val nextBoard = board.applyMove(DiscardMeat(Soja, Pork))
    nextBoard.drawStack.empty shouldBe true
    nextBoard.hands.head.cards should contain theSameElementsAs Seq(Ginger, Noodles, Chicken, Shrimp)
  }
}
