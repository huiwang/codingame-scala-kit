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
class ArenaAwarePlayer[S, A](
                           turnReader: () => () => S,
                           myPlayer: GamePlayer[S, A],
                           otherPlayer: GamePlayer[S, A],
                           arena: GameArena[S, A]
                         ) {
  var predicated: S = _

  def run(): Unit = {
    val nextTurn = turnReader()
    while (true) {
      val state: S = nextTurn()
      if (predicated != null) {
        System.err.println(predicated)
      }
      System.err.println(state)
      predicated = GameSimulator.singleTurn(state, arena, Vector(myPlayer, otherPlayer))
      myPlayer.reactTo(state).foreach(a => println(a))
    }
  }


}
