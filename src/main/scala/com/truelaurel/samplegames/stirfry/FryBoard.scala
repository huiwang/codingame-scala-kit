package com.truelaurel.samplegames.stirfry

import com.truelaurel.algorithm.game.GameState
import com.truelaurel.collection.IterableUtil._

case class FryBoard(drawStack: CardStack[Card],
                    players: Seq[FryPlayer],
                    discardStack: CardStack[Card] = CardStack(),
                    nextPlayer: Int = 0)

  extends GameState[Int] {

  def hands: Seq[CardStack[Card]] = players.map(_.hand)

  def scores: Seq[Int] = players.map(_.score)

  def draw(n: Int): FryBoard = {
    val (newCards, newDraw) = drawStack.take(n)
    updatePlayer(_.draw(newCards)).copy(drawStack = newDraw)
  }

  def discard(cards: List[Card]): FryBoard = {
    updatePlayer(_.discard(cards))
  }

  def mark(n: Int): FryBoard = copy().updatePlayer(_.mark(n))

  private def updatePlayer(f: FryPlayer => FryPlayer): FryBoard =
    copy(players = players.updatef(nextPlayer, f))

  def passTurn: FryBoard = {
    val newDraw = drawStack.addAll(discardStack.cards).shuffle
    copy(drawStack = newDraw, discardStack = CardStack(), nextPlayer = (nextPlayer + 1) % players.size)
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
    FryBoard(drawStack, players = hands.map(h => FryPlayer(h, 0)))
}

