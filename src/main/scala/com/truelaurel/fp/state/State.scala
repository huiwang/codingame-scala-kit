package com.truelaurel.fp.state

trait State[S, A] {
  val run: S => (S, A)

  def eval(s: S): A =
    apply(s)._2

  def apply(s: S): (S, A) =
    run(s)

  def map[B](f: A => B): State[S, B] =
    State { s: S =>
      val (s1, a) = run(s)
      (s1, f(a))
    }

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State { s: S =>
      val (s1, a) = run(s)
      f(a)(s1)
    }
}

object State {

  def state[S, A](a: A): State[S, A] = State { s: S => (s, a) }

  def apply[S, A](f: S => (S, A)): State[S, A] =
    new State[S, A] {
      final val run = f
    }

  def get[S]: State[S, S] = State { s: S => (s, s) }
  def gets[S, A](f: S => A): State[S, A] = State { s: S => (s, f(s)) }
  def modify[S](f: S => S): State[S, Unit] = State { s: S => (f(s), ()) }

  def reduce[S, A](states: Iterable[State[S, A]]): State[S, A] =
    sequence(states).map(_.last)

  def sequence[S, B](fs: Iterable[State[S, B]]): State[S, Vector[B]] =
    State(s => sequence_(fs, Vector(), s))
  private def sequence_[S, B](
      fs: Iterable[State[S, B]],
      out: Vector[B],
      nextS: S
  ): (S, Vector[B]) =
    if (fs.isEmpty) (nextS, out)
    else {
      val (h, t) = (fs.head, fs.tail)
      val (s, a) = h(nextS)
      sequence_(t, out :+ a, s)
    }
}
