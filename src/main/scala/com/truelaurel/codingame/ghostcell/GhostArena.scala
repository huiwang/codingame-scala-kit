package com.truelaurel.codingame.ghostcell

import com.truelaurel.codingame.engine.GameArena

/**
  * Created by hwang on 25/03/2017.
  */
object GhostArena extends GameArena[GhostCellGameState, GhostCellAction]{
  override def execute(fromState: GhostCellGameState, actions: Vector[GhostCellAction]): GhostCellGameState = {
    ???
  }
}
