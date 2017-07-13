package com.truelaurel.codingame.challenge

import com.truelaurel.codingame.logging.CGLogger


class GameLoop[Context, State, Action](
                                        gameIO: GameIO[Context, State, Action],
                                        myPlayer: GameBot[State, Action],
                                        accumulator: GameAccumulator[Context, State, Action],
                                        turns: Int = 200
                                      ) {
  def run(): Unit = {
    val time = System.nanoTime()
    val initContext = gameIO.readContext
    CGLogger.info("GameInit elt: " + (System.nanoTime() - time) / 1000000 + "ms")
    (1 to turns).foldLeft(initContext) {
      case (c, turn) =>
        val state = gameIO.readState(turn, c)
        CGLogger.info(state)
        val time = System.nanoTime()
        val actions = myPlayer.react(state)
        CGLogger.info("GameReact elt: " + (System.nanoTime() - time) / 1000000 + "ms")
        gameIO.writeAction(state, actions)
        accumulator.accumulate(c, state, actions)
    }
  }

}
