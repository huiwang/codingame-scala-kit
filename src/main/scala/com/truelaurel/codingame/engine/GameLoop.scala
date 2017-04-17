package com.truelaurel.codingame.engine


class GameLoop[C, S, A](
                         controller: GameController[C, S, A],
                         myPlayer: GamePlayer[S, A]
                       ) {
  def run(): Unit = {
    val start = System.nanoTime()
    val initContext = controller.readContext
    controller.warmup(myPlayer)
    System.err.println("init " + (System.nanoTime() - start))
    (1 to 200).foldLeft(initContext) {
      case (c, turn) =>
        val state = controller.readState(turn, c)
        System.err.println(state)
        val roundStart = System.nanoTime()
        val actions = myPlayer.reactTo(state)
        System.err.println("round time " + (System.nanoTime() - roundStart) / 1000000)
        actions.foreach(a => println(a))
        controller.nextContext(c, state, actions)
    }
  }

}
