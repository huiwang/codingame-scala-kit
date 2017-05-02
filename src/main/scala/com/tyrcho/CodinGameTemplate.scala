package com.tyrcho

import scala.Vector

object CodinGameTemplate {

  implicit class MinMaxOptionIterable[T](it: Iterable[T]) {
    def minOption[O: Ordering](by: T => O): Option[T] =
      if (it.isEmpty) None
      else Some(it.minBy(by))

    def maxOption[O: Ordering](by: T => O): Option[T] =
      if (it.isEmpty) None
      else Some(it.maxBy(by))

    def uniqueBy[O](by: T => O): Vector[T] = {
      val (k, s) = it.foldLeft((Vector.empty[T], Set.empty[O])) {
        case ((kept, seen), elt) =>
          val key = by(elt)
          if (seen(key)) (kept, seen)
          else (kept :+ elt, seen + key)
      }
      k
    }
  }

  def sqr(x: Double) = x * x

  def debug(m: Any) = Console.err.println(m)


  def median(elts: Iterable[Float]) = {
    val sorted = elts.toVector.sorted
    val s = elts.size
    if (s % 2 != 0) sorted((s - 1) / 2)
    else (sorted(s / 2) + sorted((s - 1) / 2)) / 2
  }
}