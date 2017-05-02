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

  trait State[S, A] {
    val run: S => (S, A)

    def apply(s: S): (S, A) =
      run(s)

    def eval(s: S): A =
      apply(s)._2

    def map[B](f: A => B): State[S, B] = State { s: S =>
      val (s1, a) = run(s)
      (s1, f(a))
    }

    def flatMap[B](f: A => State[S, B]): State[S, B] = State { s: S =>
      val (s1, a) = run(s)
      f(a)(s1)
    }
  }

  object State {

    def apply[S, A](f: S => (S, A)): State[S, A] = new State[S, A] {
      final val run = f
    }

    def state[S, A](a: A): State[S, A] = State { s: S => (s, a) }
    def get[S]: State[S, S] = State { s: S => (s, s) }
    def gets[S, A](f: S => A): State[S, A] = State { s: S => (s, f(s)) }
    def modify[S](f: S => S): State[S, Unit] = State { s: S => (f(s), ()) }

    def reduce[S, A](states: Iterable[State[S, A]]): State[S, A] =
      sequence(states).map(_.last)

    def sequence[S, B](fs: Iterable[State[S, B]]): State[S, Vector[B]] = State(s => sequence_(fs, Vector(), s))
    private def sequence_[S, B](fs: Iterable[State[S, B]], out: Vector[B], nextS: S): (S, Vector[B]) =
      if (fs.isEmpty) (nextS, out)
      else {
        val (h, t) = (fs.head, fs.tail)
        val (s, a) = h(nextS)
        sequence_(t, out :+ a, s)
      }
  }

  def median(elts: Iterable[Float]) = {
    val sorted = elts.toVector.sorted
    val s = elts.size
    if (s % 2 != 0) sorted((s - 1) / 2)
    else (sorted(s / 2) + sorted((s - 1) / 2)) / 2
  }
}