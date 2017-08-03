package com.truelaurel.fp

object Unfold {
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case None => Stream.empty
      case Some((elt, state)) =>
        elt #:: unfold(state)(f)
    }
}
