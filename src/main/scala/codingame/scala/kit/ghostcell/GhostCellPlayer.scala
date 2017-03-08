package codingame.scala.kit.ghostcell

import codingame.scala.kit.engine.GamePlayer

object GhostCellPlayer extends GamePlayer[GhostCellGameState, Vector[GhostCellAction]] {
  override def reactTo(state: GhostCellGameState): Vector[GhostCellAction] = {
    val attackPlan = FactoryAnalysis.movePlans(state)
    val attackMoves = attackPlan.map(m => {
      m.copy(to = state.itineraries(m.from)(m.to).path.tail.head)
    })
    withBombPlan(state, attackMoves)
  }

  private def withBombPlan(state: GhostCellGameState, moves: Vector[MoveAction]) = {
    val nextTroops = state.troops ++ FactoryAnalysis.movesToTroops(moves, state)
    moves ++ bombPlan(state, nextTroops)
  }

  private def bombPlan(state: GhostCellGameState, nextTroops: Vector[Troop]): Vector[BombAction] = {
    if (state.myFacs.isEmpty || state.otherFacs.isEmpty) Vector.empty else {
      findFront(state).map(front => {
        state.otherFacs
          .map(of => FactoryTimeline.finalState(of, nextTroops, state.dist(front.id, of.id) + 1))
          .filter(fs => fs.owner == -1)
          .sortBy(fs => (state.factories(fs.id).production * -1, state.dist(front.id, fs.id)))
          .map(fs => BombAction(front.id, fs.id))
      }).getOrElse(Vector.empty)
    }
  }

  private def findFront(state: GhostCellGameState): Option[Factory] = {
    if (state.center.mine) Some(state.center) else {
      None
    }
  }


}