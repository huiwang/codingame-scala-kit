package com.truelaurel.codingame.challenge

import com.truelaurel.codingame.logging.CGLogger

object GameLoop {
  /**
    * @param myPlayer         implementation of strategy
    * @param accumulator      defines how state can keep hidden information like fog of war
    * @param turns            max turns to play
    * @param readInitialState called at start to read the configuration from the referee
    * @param readState        reads current state from the referee system. A state provides information for the current turn
    * @param writeAction      to the referee system
    */
  def run[State, Action](
                          readInitialState: () => State,
                          readState: (Int, State) => State,
                          writeAction: Action => Unit,
                          myPlayer: State => Vector[Action],
                          accumulator: (State, Vector[Action]) => State = defaultAccumulator[State, Action] _,
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
        val actions = myPlayer(state)
        CGLogger.info("GameReact elt: " + (System.nanoTime() - time) / 1000000 + "ms")
        actions.foreach(writeAction)
        accumulator(state, actions)
    }
  }

  def defaultAccumulator[State, Action](s: State, a: Vector[Action]): State = s

}

