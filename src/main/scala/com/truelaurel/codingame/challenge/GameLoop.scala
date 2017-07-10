package com.truelaurel.codingame.challenge

import com.truelaurel.codingame.logging.CGLogger


class GameLoop[State, Action](
                               gameIO: GameIO[State, Action],
                               myPlayer: State => Vector[Action],
                               accumulator: (State, Vector[Action]) => State,
                               turns: Int = 200
                             ) {
  def run(): Unit = {
    val time = System.nanoTime()
    val initialState = gameIO.readInitialState
    CGLogger.info("GameInit elt: " + (System.nanoTime() - time) / 1000000 + "ms")
    (1 to turns).foldLeft(initialState) {
      case (s, turn) =>
        val state = gameIO.readState(turn, s)
        CGLogger.info(state)
        val time = System.nanoTime()
        val actions = myPlayer(state)
        CGLogger.info("GameReact elt: " + (System.nanoTime() - time) / 1000000 + "ms")
        actions.foreach(a => gameIO.writeAction(a))
        accumulator(state, actions)
    }
  }

}
