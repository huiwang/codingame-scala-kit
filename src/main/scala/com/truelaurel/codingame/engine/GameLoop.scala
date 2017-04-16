package com.truelaurel.codingame.engine


class GameLoop[C, S, A](
                         controller: GameController[C, S, A],
                         myPlayer: GamePlayer[S, A]
                       ) {
  def run(): Unit = {
    val start = System.nanoTime()
    val initContext = controller.readContext
    System.err.println("init " + (System.nanoTime() - start))
    (1 to 200).foldLeft(initContext) {
      case (c, turn) =>
        val state = controller.readState(turn, c)
        System.err.println(state)
        val actions = myPlayer.reactTo(state)
        actions.foreach(a => println(a))
        controller.nextContext(c, state, actions)
    }
  }

}
