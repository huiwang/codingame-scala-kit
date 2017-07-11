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



}

object FryBoard {
  def apply(hands: Seq[CardStack[Card]], drawStack: CardStack[Card]): FryBoard =
    FryBoard(hands, drawStack, scores = Seq.fill(hands.size)(0))
}

