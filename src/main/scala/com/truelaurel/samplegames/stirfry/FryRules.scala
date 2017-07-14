package com.truelaurel.samplegames.stirfry

import com.truelaurel.algorithm.game._

case object FryRules extends GameRules[Int, FryBoard, FryMove] {

  def validMoves(state: FryBoard): Seq[FryMove] = state.validMoves

  def applyMove(state: FryBoard, move: FryMove): FryBoard =
    move match {
      case DiscardPair(c1, c2) => state.discard(List(c1, c2)).draw(3)
      case DiscardMeat(c, meat) => state.discard(List(c)).draw(meat.meatValue.get)
      case Cook(meal) => state.discard(meal.toList).draw(1).mark(Card.score(meal.toSeq)).passTurn
      case Pass => state.passTurn
    }

  def outcome(b: FryBoard) =
    Undecided

  def initial: FryBoard = ???

  def isValidMeal(cards: Seq[Card]): Boolean =
    cards.size > 2 && cards.size < 6 && cards.contains(Noodles) && cards.distinct.size == cards.size
}