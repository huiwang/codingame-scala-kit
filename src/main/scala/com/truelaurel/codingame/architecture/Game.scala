package com.truelaurel.codingame.architecture

trait GameState[A] {
  def apply(action: A): GameState[A]
}

trait GamePlayer[S, A] {
  def reactTo(state: S): Vector[A]
}

trait GameArena[S, A] {
  def next(fromState: S, actions: Vector[A]): S

  def judge(state: S): GameResult
}
