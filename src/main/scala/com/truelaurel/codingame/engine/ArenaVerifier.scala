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
abstract class ArenaVerifier[S, A] {
  var predicated: S = _

  def run(): Unit = {
    preGame()
    while (true) {
      val state: S = readState()
      if (predicated != null) {
        System.err.println(predicated)
      }
      System.err.println(state)
      predicated = GameSimulator.singleTurn(state, arena(), Vector(mePlayer(), otherPlayer()))
      mePlayer().reactTo(state).foreach(a => println(a))
    }
  }

  /**
    * Gets my player. Usually, my player and other player shares the same implementation.
    * They just have different parameters to play the opposite roles.
    */
  abstract def mePlayer(): GamePlayer[S, A]

  abstract def otherPlayer(): GamePlayer[S, A]

  /**
    * Get my arena which can predict the next game state
    */
  abstract def arena(): GameArena[S, A]

  /**
    * Read the game before the first turn
    */
  abstract def preGame(): Unit

  /**
    * Reads the current state
    */
  abstract def readState(): S
}
