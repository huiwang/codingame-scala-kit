package com.truelaurel.codingame.game


class GameLoop[C, S, A](
                         controller: GameController[C, S, A],
                         myPlayer: GamePlayer[S, A]
                       ) {
  def run(): Unit = {
    val time = System.nanoTime()
    val initContext = controller.readContext
    controller.warmup(myPlayer)
    System.err.println("GameInit elt: " + (System.nanoTime() - time) / 1000000 + "ms")
    (1 to 200).foldLeft(initContext) {
      case (c, turn) =>
        val state = controller.readState(turn, c)
        System.err.println(state)
        val time = System.nanoTime()
        val actions = myPlayer.reactTo(state)
        System.err.println("GameReact elt: " + (System.nanoTime() - time) / 1000000 + "ms")
        actions.foreach(a => println(a))
        val context = controller.nextContext(c, state, actions)
        context
    }
  }

}
