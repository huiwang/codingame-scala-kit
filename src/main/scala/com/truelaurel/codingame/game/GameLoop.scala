package com.truelaurel.codingame.game

import com.truelaurel.codingame.logging.CGLogger


class GameLoop[C, S, A](
                         controller: GameController[C, S, A],
                         myPlayer: GamePlayer[S, A],
                         turns: Int = 200
                       ) {
  def run(): Unit = {
    val time = System.nanoTime()
    val initContext = controller.readContext
    controller.warmup(myPlayer)
    CGLogger.info("GameInit elt: " + (System.nanoTime() - time) / 1000000 + "ms")
    (1 to turns).foldLeft(initContext) {
      case (c, turn) =>
        val state = controller.readState(turn, c)
        CGLogger.info(state)
        val time = System.nanoTime()
        val actions = myPlayer.reactTo(state)
        CGLogger.info("GameReact elt: " + (System.nanoTime() - time) / 1000000 + "ms")
        actions.foreach(a => println(a))
        controller.nextContext(c, state, actions)
    }
  }

}
