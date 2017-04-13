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
class GameLoop[C, S, A](
                         contextReader: () => C,
                         stateReader: (Int, C) => S,
                         contextEvolver: (C, S) => C,
                         myPlayer: GamePlayer[S, A]
                       ) {
  def run(): Unit = {
    val context = contextReader()
    (0 until 200).foldLeft(context) {
      case (c, turn) =>
        val state = stateReader(turn, c)
        myPlayer.reactTo(state).foreach(a => println(a))
        contextEvolver(c, state)
    }
  }

}
