package com.truelaurel.codingame.engine

trait GameState[A] {
  def apply(action : A) : GameState[A]
}

trait GamePlayer[S, A] {
  def reactTo(state: S): A
}

trait GameEngine[S, A] {
  def execute(fromState: S, actions: List[A]): S
}

object GameSimulator {

  /**
    * Simulate the game for n round with all players and a game engine
    */
  def simulate[S, A](round: Int, from: S, engine: GameEngine[S, A], players: List[GamePlayer[S, A]]): S = {
    (0 until round).foldLeft(from)((s, r) => engine.execute(s, players.map(_.reactTo(s))))
  }

}