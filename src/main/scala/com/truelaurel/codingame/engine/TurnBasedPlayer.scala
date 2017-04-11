package com.truelaurel.codingame.engine

/**
  * Verifies that local arena with the following steps:
  * <ol>
  * <li> Create a deterministic player and submit to arena
  * <li> Play this player against the one in arena
  * <li> Predict the next game state with the predictable players
  * <li> Compare the predicted state and the actual state to make sure the arena is simulating the game correctly
  * </ol>
  */
class TurnBasedPlayer[S, A](
                             gameReader: () => () => S,
                             myPlayer: GamePlayer[S, A]
                           ) {
  def run(): Unit = {
    val turnReader = gameReader()
    while (true) {
      val state: S = turnReader()
      myPlayer.reactTo(state).foreach(a => println(a))
    }
  }

}
