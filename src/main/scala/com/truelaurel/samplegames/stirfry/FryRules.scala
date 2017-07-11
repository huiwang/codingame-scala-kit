package com.truelaurel.samplegames.stirfry

import com.truelaurel.algorithm.game._
import com.truelaurel.math.geometry.Pos

case object FryRules extends RulesFor2p[FryBoard, Pos] {


  def validMoves(state: FryBoard): Seq[Pos] = ???

  def applyMove(state: FryBoard, move: Pos): FryBoard =
    ???

  def outcome(b: FryBoard) =
    Undecided

  def initial: FryBoard = ???

  def isValidMeal(cards: Seq[Card]): Boolean =
    cards.size > 2 && cards.size < 6 && cards.contains(Noodles) && cards.distinct.size == cards.size
}