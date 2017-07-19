package com.truelaurel.samplegames.stirfry

import com.truelaurel.algorithm.game.GameState
import com.truelaurel.collection.IterableUtil._

case class FryBoard(drawStack: CardStack[Card],
                    players: Seq[FryPlayer],
                    discardStack: CardStack[Card] = CardStack(),
                    nextPlayer: Int = 0,
                    hasDiscardedPair: Boolean = false,
                    hasDiscardedMeat: Boolean = false)

  extends GameState[Int] {

  def hands: Seq[CardStack[Card]] = players.map(_.hand)

  def scores: Seq[Int] = players.map(_.score)

  def draw(n: Int): FryBoard = {
    val (newCards, newDraw) = drawStack.take(n)
    updatePlayer(_.draw(newCards)).copy(drawStack = newDraw)
  }

  def discard(cards: List[Card]): FryBoard = {
    updatePlayer(_.discard(cards)).copy(discardStack = discardStack.addAll(cards))
  }

  def winner: Option[Int] = players.zipWithIndex.collect {
    case (p, i) if p.score >= 50 => i
  }.headOption

  def mark(n: Int): FryBoard = copy().updatePlayer(_.mark(n))

  private def updatePlayer(f: FryPlayer => FryPlayer): FryBoard =
    copy(players = players.updatef(nextPlayer, f))

  def passTurn: FryBoard = {
    val newDraw = drawStack.addAll(discardStack.cards).shuffle
    copy(
      drawStack = newDraw,
      discardStack = CardStack(),
      nextPlayer = (nextPlayer + 1) % players.size,
      hasDiscardedMeat = false,
      hasDiscardedPair = false).draw(1)
  }

  def validMoves: List[FryMove] =
    discardPairMoves ::: discardMeatMoves ::: cookMoves ::: passMoves

  private def passMoves: List[Pass] = {
    val hand = hands(nextPlayer).cards
    if (hand.size <= 3) List(Pass())
    else hand.combinations(hand.size - 3).map(Pass).toList
  }

  private def discardMeatMoves: List[DiscardMeat] =
    if (hasDiscardedMeat) Nil
    else for {
      actual <- hands(nextPlayer).cards
      meat <- Card.meats
    } yield DiscardMeat(actual, meat)

  private def discardPairMoves: List[DiscardPair] =
    if (hasDiscardedPair) Nil
    else hands(nextPlayer).cards.combinations(2).map {
      case (List(a, b)) => DiscardPair(a, b)
    }.toList

  private def cookMoves: List[Cook] = if (hands(nextPlayer).cards.contains(Noodles)) {
    val rest = hands(nextPlayer).cards.filter(Noodles.!=).distinct
    (rest.combinations(2) ++ rest.combinations(3) ++ rest.combinations(4)).map(cards => Cook((Noodles :: cards).toSet)).toList
  } else Nil

}

object FryBoard {
  def apply(hands: Seq[CardStack[Card]], drawStack: CardStack[Card]): FryBoard =
    FryBoard(drawStack, players = hands.map(h => FryPlayer(h, 0)))
}

