package com.truelaurel.samplegames.stirfry

import com.truelaurel.algorithm.game._

case class FryRules(players: Int = 2) extends GameRules[Int, FryBoard, FryMove] {

  def validMoves(state: FryBoard): Seq[FryMove] = state.validMoves

  def applyMove(state: FryBoard, move: FryMove): FryBoard =
    move match {
      case DiscardPair(c1, c2) => state.discard(List(c1, c2)).draw(3).copy(hasDiscardedPair = true)
      case DiscardMeat(c, meat) => state.discard(List(c)).draw(meat.meatValue.get).copy(hasDiscardedMeat = true)
      case Cook(meal) => state.discard(meal.toList).draw(1).mark(Card.score(meal.toSeq)).passTurn
      case Pass => state.passTurn
    }

  def outcome(b: FryBoard): Outcome[Int] =
    if (b.scores(b.nextPlayer) >= 50) Wins(b.nextPlayer)
    else Undecided

  def initial: FryBoard = {
    val draw = CardStack(Chicken :: Pork :: Shrimp ::
      List.fill(5)(Noodles) :::
      List.fill(2)(Ginger) :::
      List.fill(2)(Onion) :::
      List.fill(3)(Mushrooms) :::
      List.fill(3)(Soja)).shuffle

    val (playerHands, drawStack) = (1 to players).foldLeft((List.empty[FryPlayer], draw)) {
      case ((p, cards), _) =>
        val (hand, rest) = cards.take(3)
        (FryPlayer(CardStack(hand)) :: p, rest)
    }
    FryBoard(drawStack, playerHands)
  }

  def isValidMeal(cards: Seq[Card]): Boolean =
    cards.size > 2 && cards.size < 6 && cards.contains(Noodles) && cards.distinct.size == cards.size
}