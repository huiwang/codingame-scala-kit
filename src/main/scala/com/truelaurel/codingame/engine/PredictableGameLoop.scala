package com.truelaurel.codingame.engine

class PredictableGameLoop[C, S, A](
                                    controller: GameController[C, S, A],
                                    myPlayer: GamePlayer[S, A],
                                    otherPlayer: GamePlayer[S, A],
                                    arena: GameArena[S, A]
                                  ) {
  var predicated: S = _

  def run(): Unit = {
    val initContext = controller.readContext
    (1 to 200).foldLeft(initContext) {
      case (c, turn) =>
        val state = controller.readState(turn, c)
        if (predicated != null) {
          System.err.println(predicated)
        }
        System.err.println(state)
        val actions = myPlayer.reactTo(state)
        predicated = GameSimulator.singleTurn(state, arena, Vector(myPlayer, otherPlayer))
        actions.foreach(a => println(a))
        controller.nextContext(c, state, actions)
    }
  }

}
