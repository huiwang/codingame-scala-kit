package com.truelaurel.codingame.engine


class GameLoop[C, S, A](
                         controller: GameController[C, S, A],
                         myPlayer: GamePlayer[S, A]
                       ) {
  def run(): Unit = {
    val time = System.nanoTime()
    val initContext = controller.readContext
    controller.warmup(myPlayer)
    System.err.println("gameInit elt: " + (System.nanoTime() - time) / 1000000 + "ms")
    (1 to 200).foldLeft(initContext) {
      case (c, turn) =>
        val time = System.nanoTime()
        val time2 = System.nanoTime()
        val state = controller.readState(turn, c)
        System.err.println("turnRead elt: " + (System.nanoTime() - time2) / 1000000 + "ms")
        System.err.println(state)
        val time3 = System.nanoTime()
        val actions = myPlayer.reactTo(state, System.nanoTime() - time)
        System.err.println("turnReact elt: " + (System.nanoTime() - time3) / 1000000 + "ms")
        actions.foreach(a => println(a))
        val context = controller.nextContext(c, state, actions)
        System.err.println("turnTotal elt: " + (System.nanoTime() - time) / 1000000 + "ms")
        context
    }
  }

}
