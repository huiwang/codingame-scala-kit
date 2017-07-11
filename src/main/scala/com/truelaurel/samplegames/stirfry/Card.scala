package com.truelaurel.samplegames.stirfry

object Card {
  def score(meal: Seq[Card]) =
    meal.map(_.score(meal)).sum

  def all: Set[Card] = Set(Noodles, Mushrooms, Soja, Ginger, Onion, Chicken, Pork, Shrimp)
}

sealed trait Card {
  def score(others: Seq[Card]): Int

  val meatValue: Option[Int] = None
  lazy val meat: Boolean = meatValue.isDefined
}

case object Noodles extends Card {
  def score(others: Seq[Card]) = 1
}

case object Mushrooms extends Card {
  def score(others: Seq[Card]): Int =
    if (others.contains(Chicken)) 3 else 2
}

case object Soja extends Card {
  def score(others: Seq[Card]): Int =
    if (others.contains(Ginger)) 3
    else if (others.contains(Onion)) 2
    else 1
}

case object Ginger extends Card {
  def score(others: Seq[Card]): Int =
    if (others.contains(Pork)) 4 else 2
}

case object Onion extends Card {
  def score(others: Seq[Card]): Int =
    if (others.contains(Shrimp)) 5
    else 3
}

case object Chicken extends Card {
  def score(others: Seq[Card]): Int =
    if (others.contains(Onion) && others.contains(Ginger)) 7
    else if (others.contains(Onion)) 5
    else 2

  override val meatValue = Some(2)
}

case object Pork extends Card {
  def score(others: Seq[Card]): Int =
    if (others.contains(Mushrooms)) 8
    else 5

  override val meatValue = Some(3)
}

case object Shrimp extends Card {
  def score(others: Seq[Card]): Int =
    if (others.contains(Soja) && others.contains(Ginger)) 11
    else if (others.contains(Ginger)) 9
    else 6

  override val meatValue = Some(4)
}
