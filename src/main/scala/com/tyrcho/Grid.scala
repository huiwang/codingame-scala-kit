package com.tyrcho

import com.tyrcho.CodinGameTemplate._

case class Grid(links: Vector[Link]) {
  val ids: Set[Int] = links.toSet[Link].flatMap(l => Set(l.factory1, l.factory2))

  val distances: Map[Int, Map[Int, Int]] =
    ids.map { from =>
      from -> (for {
        to <- ids.diff(Set(from))
        link <- links.find(_.isFor(from, to))
        d = link.distance
      } yield to -> d).toMap
    }.toMap

  def distance(f1: Int, f2: Int) = if (f1 == f2) 0 else distances(f1)(f2)

  def nearestStop(from: Int, to: Int): Option[Int] =
    stops(from, to).minOption(s => distance(from, s))

  def stops(f1: Int, f2: Int) =
    ids.diff(Set(f1, f2))
      .filter(i => distance(f1, i) + distance(f2, i) + 1 <= distance(f1, f2))

  def bombCanReachFactoriesThisTurn(bomb: Bomb): Set[Int] = {
    ids.filter(i => distance(i, bomb.from) == bomb.time)
  }
}

case class Link(factory1: Int, factory2: Int, distance: Int) {
  def isFor(f1: Int, f2: Int) =
    (f1 == factory1 && f2 == factory2) || (f2 == factory1 && f1 == factory2)

  override def toString = s"$factory1-$factory2:$distance"
}