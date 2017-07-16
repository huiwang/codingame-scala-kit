package com.truelaurel.samplegames.stirfry

import org.scalatest.{FlatSpec, Matchers}

class FryRulesTest extends FlatSpec with Matchers {
  val rules = FryRules(2)

  "Fry rules" should "handle discard 2" in {
    val board = FryBoard(
      hands = Seq(CardStack(List(Noodles, Noodles))),
      drawStack = CardStack(List(Pork, Chicken, Shrimp)))

    val nextBoard = rules.applyMove(board, DiscardPair(Noodles, Noodles))
    nextBoard.drawStack.empty shouldBe true
    nextBoard.discardStack.cards should contain theSameElementsAs Seq(Noodles, Noodles)
    nextBoard.hands.head.cards should contain theSameElementsAs Seq(Pork, Chicken, Shrimp)
  }

  it should "handle discard pork" in {
    val board = FryBoard(
      hands = Seq(CardStack(List(Soja, Noodles))),
      drawStack = CardStack(List(Ginger, Chicken, Shrimp)))

    val nextBoard = rules.applyMove(board, DiscardMeat(Soja, Pork))
    nextBoard.drawStack.empty shouldBe true
    nextBoard.discardStack.cards should contain theSameElementsAs Seq(Soja)
    nextBoard.hands.head.cards should contain theSameElementsAs Seq(Ginger, Noodles, Chicken, Shrimp)
  }

  it should "score and draw for cook" in {
    val board = FryBoard(hands = Seq(CardStack(List(Soja, Noodles, Chicken)), CardStack()), drawStack = CardStack(List(Pork)))

    val nextBoard = rules.applyMove(board, Cook(Set(Soja, Noodles, Chicken)))
    nextBoard.hands.head.cards should contain theSameElementsAs Seq(Pork)
    nextBoard.scores.head shouldBe 4
    nextBoard.nextPlayer shouldBe 1
  }

  it should "discard to 3 then shuffle at end of turn" in {
    val board = FryBoard(
      drawStack = CardStack(List(Pork, Soja)),
      players = Seq(FryPlayer(CardStack(List(Noodles, Noodles, Noodles, Noodles))), FryPlayer()),
      discardStack = CardStack(List(Chicken)))

    val nextBoard = rules.applyMove(board, Pass(List(Noodles)))
    nextBoard.players.head.hand.cards.size shouldBe 3
    nextBoard.drawStack.cards.size shouldBe 3
    nextBoard.discardStack.empty shouldBe true
  }

  it should "list all possible valid moves" in {
    val board = FryBoard(
      hands = Seq(CardStack(List(Noodles, Soja, Onion, Chicken))),
      drawStack = CardStack())

    val moves = rules.validMoves(board)
    moves.collect { case Pass(discards) => discards.head } should contain theSameElementsAs List(Noodles, Soja, Onion, Chicken)
    moves.count { case DiscardMeat(Onion, _) => true; case _ => false } shouldBe 3
    moves.count { case DiscardMeat(_, Pork) => true; case _ => false } shouldBe 4
    val allPairs = List(Noodles, Soja, Onion, Chicken).combinations(2).toList
    moves.collect { case DiscardPair(a, b) => List(a, b) } should contain theSameElementsAs allPairs
    moves.count { case Cook(_) => true; case _ => false } shouldBe 4
  }

  it should "not allow to discard meat twice" in {
    val board = FryBoard(
      hands = Seq(CardStack(List(Noodles))),
      drawStack = CardStack(List(Noodles, Soja, Onion, Chicken)))

    val movesAfterDiscardMeat = rules.validMoves(rules.applyMove(board, DiscardMeat(Noodles, Chicken)))
    movesAfterDiscardMeat.exists { case _: DiscardMeat => true; case _ => false } shouldBe false
    movesAfterDiscardMeat.exists { case _: DiscardPair => true; case _ => false } shouldBe true
  }

  it should "not allow to discard pair twice" in {
    val board = FryBoard(
      hands = Seq(CardStack(List(Noodles, Soja))),
      drawStack = CardStack(List(Noodles, Soja, Onion, Chicken)))

    val movesAfterDiscard = rules.validMoves(rules.applyMove(board, DiscardPair(Noodles, Soja)))
    movesAfterDiscard.exists { case _: DiscardMeat => true; case _ => false } shouldBe true
    movesAfterDiscard.exists { case _: DiscardPair => true; case _ => false } shouldBe false
  }

  it should "not allow to discard pair with 1 card" in {
    val board = FryBoard(
      hands = Seq(CardStack(List(Noodles))),
      drawStack = CardStack(List(Noodles, Soja, Onion, Chicken)))

    val moves = rules.validMoves(board)
    moves.exists { case _: DiscardMeat => true; case _ => false } shouldBe true
    moves.exists { case _: DiscardPair => true; case _ => false } shouldBe false
  }
}
