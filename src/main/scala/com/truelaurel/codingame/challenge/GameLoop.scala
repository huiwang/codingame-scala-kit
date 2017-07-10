package com.truelaurel.codingame.challenge

import com.truelaurel.codingame.logging.CGLogger

/**
  * Glues together the stragegy (myPlayer) and IO related to CodinGame referee.
  */
object GameLoop {
  /**
    * Runs the given number of turns, interacting with CodinGame referee through IO.
    *
    * @param myPlayer         implementation of strategy
    * @param accumulator      defines how state can keep hidden information like fog of war
    * @param turns            max turns to play
    * @param readInitialState called at start to read the configuration from the referee
    * @param readState        reads current state from the referee system. A state provides information for the current turn
    * @param writeAction      to the referee system
    * @tparam S related game state
    * @tparam A action or actions; can be chosen by the player and applied to the game state
    */
  def run[S, A](
                 readInitialState: () => S,
                 readState: (Int, S) => S,
                 writeAction: A => Unit,
                 myPlayer: S => A,
                 accumulator: (S, A) => S = defaultAccumulator[S, A] _,
                 turns: Int = 200
               ): Unit = {

    val time = System.nanoTime()
    val initialState = readInitialState()
    CGLogger.info("GameInit elt: " + (System.nanoTime() - time) / 1000000 + "ms")
    (1 to turns).foldLeft(initialState) {
      case (s, turn) =>
        val state = readState(turn, s)
        CGLogger.info(state)
        val time = System.nanoTime()
        val action = myPlayer(state)
        CGLogger.info("GameReact elt: " + (System.nanoTime() - time) / 1000000 + "ms")
        writeAction(action)
        accumulator(state, action)
    }
  }

  def defaultAccumulator[State, Action](s: State, a: Action): State = s

}

