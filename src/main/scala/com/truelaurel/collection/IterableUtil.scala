package com.truelaurel.collection

import scala.collection.SeqLike
import scala.collection.generic.CanBuildFrom

object IterableUtil {

  implicit class NumericIterableOps(elts: Iterable[Float]) {
    def median = {
      val sorted = elts.toVector.sorted
      val s = elts.size
      if (s % 2 != 0) sorted((s - 1) / 2)
      else (sorted(s / 2) + sorted((s - 1) / 2)) / 2
    }
  }

  implicit class IterableOrderedOps[T: Ordering](it: Iterable[T]) {
    def maxOpt: Option[T] =
      if (it.isEmpty) None
      else Some(it.max)

    def minOpt: Option[T] =
      if (it.isEmpty) None
      else Some(it.min)
  }


  implicit class IterableOps[T](it: Iterable[T]) {
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

  implicit class SeqLikeOps[T, Repr](vector: SeqLike[T, Repr]) {

    def updateWhen[That](when: T => Boolean, f: T => T)
                        (implicit bf: CanBuildFrom[Repr, T, That]): That = {
      val index = vector.indexWhere(when)
      updatef(index, f)
    }

    def updatef[That](index: Int, f: T => T)
                     (implicit bf: CanBuildFrom[Repr, T, That]): That =
      vector.updated(index, f(vector(index)))
  }

  implicit class StringOps(str: String) {

    def updateWhen(when: Char => Boolean, f: Char => Char): String = {
      val index = str.indexWhere(when)
      updatef(index, f)
    }

    def updatef(index: Int, f: Char => Char): String =
      str.updated(index, f(str(index)))
  }

  implicit class VectorOps2[T](vector: Seq[Seq[T]]) {
    def updatef2(i: Int, j: Int, f: T => T) =
      vector.updatef(i, _.updatef(j, f))
  }

}