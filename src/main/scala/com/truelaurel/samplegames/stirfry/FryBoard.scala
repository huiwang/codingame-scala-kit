package com.truelaurel.samplegames.stirfry

import com.truelaurel.algorithm.game.GameState
import com.truelaurel.collection.IterableUtil._

case class FryBoard(hands: Seq[CardStack[Card]],
                    drawStack: CardStack[Card],
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

  def applyMove(move: FryMove) = move match {
    case DiscardPair(c1, c2) => discard(Seq(c1, c2)).draw(3)
    case DiscardMeat(c, meat) => discard(Seq(c)).draw(meat.meatValue.get)
  }

}
