package com.truelaurel.samplegames.stirfry

import com.truelaurel.algorithm.game.GameState
import com.truelaurel.collection.IterableUtil._

case class FryBoard(hands: Seq[CardStack[Card]],
                    drawStack: CardStack[Card],
                    scores: Seq[Int],
                    discardStack: CardStack[Card] = CardStack(),
                    nextPlayer: Int = 0)
  extends GameState[Int] {

  def draw(n: Int): FryBoard = {
    val (newCards, newDraw) = drawStack.take(n)
    val newHands = hands.updatef(nextPlayer, _.addAll(newCards))
    copy(hands = newHands, drawStack = newDraw)
  }

  def discard(cards: Seq[Card]): FryBoard = {
    val newHands = hands.updatef(nextPlayer, _.remove(cards))
    copy(hands = newHands)
  }

  def score(n: Int): FryBoard = copy(scores = scores.updatef(nextPlayer, n.+))

  def passTurn: FryBoard = {
    val newDraw = drawStack.addAll(discardStack.cards).shuffle
    copy(nextPlayer = (nextPlayer + 1) % hands.size,
      drawStack = newDraw,
      discardStack = CardStack())
  }

  def validMoves: List[FryMove] =
    Pass :: discardPairMoves ::: discardMeatMoves ::: cookMoves

  private def discardMeatMoves: List[DiscardMeat] = for {
    actual <- hands(nextPlayer).cards
    meat <- Card.meats
  } yield DiscardMeat(actual, meat)

  private def discardPairMoves: List[DiscardPair] = hands(nextPlayer).cards.combinations(2).map {
    case (List(a, b)) => DiscardPair(a, b)
  }.toList

  private def cookMoves: List[Cook] = if (hands(nextPlayer).cards.contains(Noodles)) {
    val rest = hands(nextPlayer).cards.filter(Noodles.!=).distinct
    (rest.combinations(2) ++ rest.combinations(3) ++ rest.combinations(4)).map(cards => Cook((Noodles :: cards).toSet)).toList
  } else Nil

}

object FryBoard {
  def apply(hands: Seq[CardStack[Card]], drawStack: CardStack[Card]): FryBoard =
    FryBoard(hands, drawStack, scores = Seq.fill(hands.size)(0))
}

