package com.truelaurel.codingame.ghostcell.common

/**
  * Created by hwang on 30/03/2017.
  */
object BombBudget {

  def computeBombBudget(actions: Vector[GhostCellAction], state: GhostCellGameState, budget: Map[Int, Int]): Map[Int, Int] = {
    for {
      (playerId, bombBudget) <- budget
      used = actions.count {
        case BombAction(from, _) => state.fac(from).owner == playerId
        case _ => false
      }
    } yield playerId -> (bombBudget - used)
  }
}
