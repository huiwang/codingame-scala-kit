package com.truelaurel.samplegames.stirfry

import scala.util.Random

case class CardStack[+T](cards: List[T] = Nil) {

  def empty: Boolean = cards.isEmpty

  def addAll[U >: T](newCards: List[U]): CardStack[U] =
    CardStack(newCards ::: cards)

  def remove[U >: T](cs: Seq[U]): CardStack[T] =
    CardStack(cards diff cs)

  def take(n: Int): (List[T], CardStack[T]) = {
    val (h, t) = cards.splitAt(n)
    (h, CardStack(t))
  }

  def all: (List[T], CardStack[T]) = take(cards.size)

  def add[U >: T](c: U): CardStack[U] =
    CardStack(c :: cards)

  def shuffle: CardStack[T] = CardStack(Random.shuffle(cards))

}
