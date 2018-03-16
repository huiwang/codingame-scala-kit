package com.truelaurel.codingame.challenge

import com.truelaurel.codingame.logging.CGLogger


class GameLoop[Context, State, Action](
                                        gameIO: GameIO[Context, State, Action],
                                        myPlayer: GameBot[State, Action],
                                        accumulator: GameAccumulator[Context, State, Action],
                                        turns: Int = 200
                                      ) {
  def run(): Unit = {
    CGLogger.startOfRound = System.nanoTime
    val initContext = gameIO.readContext
    CGLogger.info("GameInit")
    (1 to turns).foldLeft(initContext) {
      case (c, turn) =>
        val state = gameIO.readState(turn, c)
        CGLogger.startOfRound = System.nanoTime
        CGLogger.info(state)
        val actions = myPlayer.react(state)
        CGLogger.info("GameReact")
        gameIO.writeAction(state, actions)
        accumulator.accumulate(c, state, actions)
    }
  }

}
