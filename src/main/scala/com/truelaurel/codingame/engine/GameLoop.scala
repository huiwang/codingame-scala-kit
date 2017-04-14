package com.truelaurel.codingame.engine


trait GameController[C, S, A] {
  def readContext: C

  def readState(turn: Int, context: C): S

  def nextContext(context: C, state: S, actions: Vector[A]): C
}

class GameLoop[C, S, A](
                         controller: GameController[C, S, A],
                         myPlayer: GamePlayer[S, A]
                       ) {
  def run(): Unit = {
    val initContext = controller.readContext
    (0 until 200).foldLeft(initContext) {
      case (c, turn) =>
        val state = controller.readState(turn, c)
        System.err.println(c)
        System.err.println(state)
        val actions = myPlayer.reactTo(state)
        actions.foreach(a => println(a))
        controller.nextContext(c, state, actions)
    }
  }

}
