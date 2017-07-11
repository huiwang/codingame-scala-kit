package com.truelaurel.codingame.challenge

import com.truelaurel.codingame.logging.CGLogger

/**
  * Glues together the stragegy (myPlayer) and IO related to CodinGame referee.
  */
object GameLoop {
  /**
    * Runs the given number of turns, interacting with CodinGame referee through IO.
    *
    * @param myPlayer    implementation of strategy
    * @param accumulator defines how state can keep hidden information like fog of war
    * @param turns       max turns to play
    * @param io          handles read state and write actions
    * @tparam S related game state
    * @tparam A action or actions; can be chosen by the player and applied to the game state
    */
  def run[S, A](
                 io: GameIO[S, A],
                 myPlayer: S => A,
                 accumulator: (S, A) => S = defaultAccumulator[S, A] _,
                 turns: Int = 200
               ): Unit = {

    val time = System.nanoTime()
    val initialState = io.readInitialState()
    CGLogger.info("GameInit elt: " + (System.nanoTime() - time) / 1000000 + "ms")
    (1 to turns).foldLeft(initialState) {
      case (s, turn) =>
        CGLogger.info(s)
        val time = System.nanoTime()
        val action = myPlayer(s)
        CGLogger.info("GameReact elt: " + (System.nanoTime() - time) / 1000000 + "ms")
        io.writeAction(action)
        val state = accumulator(s, action)
        io.readState(turn, state)
    }
  }

  def defaultAccumulator[State, Action](s: State, a: Action): State = s

}

