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


  def evaluateOffline[S, A](games: Vector[S], arena: GameArena[S, A], players: Vector[GamePlayer[S, A]], round: Int = 200): Unit = {
    val results = games.par.map(play(_, arena, players, round))

    val wins = results.count {
      case WinKO | WinTech => true
      case _ => false
    }

    val draws = results.count {
      case Draw => true
      case _ => false
    }

    val p = wins.toDouble / games.size
    val winRate = p * 100.0

    val confidenceInterval = Math.sqrt(p * (1 - p) / games.size) * 100

    println(s"${games.size} Games $wins Wins $draws Draws")
    println(s"WinRate:$winRate%+-$confidenceInterval%")

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
