package com.truelaurel.codingame.engine

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

trait GameResult {
}

case object Draw extends GameResult

case object WinKO extends GameResult

case object LossKO extends GameResult

case object WinTech extends GameResult

case object LossTech extends GameResult

object GameSimulator {

  /**
    * Simulate the game for n round with all players and a game engine
    */
  def simulate[S, A](round: Int, from: S, arena: GameArena[S, A], players: Vector[GamePlayer[S, A]]): S = {
    (0 until round).foldLeft(from)((s, r) => arena.next(s, players.flatMap(_.reactTo(s))))
  }

  def play[S, A](from: S, arena: GameArena[S, A], players: Vector[GamePlayer[S, A]], round: Int = 200): GameResult = {
    val result = arena.judge(from)
    if (round == 0) result else {
      result match {
        case WinKO | LossKO => result
        case _ =>
          val next = arena.next(from, players.flatMap(_.reactTo(from)))
          play(next, arena, players, round - 1)
      }
    }
  }

}