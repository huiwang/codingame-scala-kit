package com.truelaurel.samplegames.stirfry


sealed trait FryMove {
}

case class DiscardPair(c1: Card, c2: Card) extends FryMove

case class DiscardMeat(actual: Card, namedMeat: Card) extends FryMove

case class Cook(meal: Set[Card]) extends FryMove

case object Pass extends FryMove