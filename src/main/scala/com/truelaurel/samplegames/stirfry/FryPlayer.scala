package com.truelaurel.samplegames.stirfry

case class FryPlayer(hand: CardStack[Card] = CardStack(), score: Int = 0) {
  def draw(cards: List[Card]): FryPlayer = copy(hand = hand.addAll(cards))

  def discard(cards: List[Card]): FryPlayer = copy(hand = hand.remove(cards))

  def mark(points: Int): FryPlayer = copy(score = score + points)
}
