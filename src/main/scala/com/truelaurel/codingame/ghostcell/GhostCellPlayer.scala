package com.truelaurel.codingame.ghostcell

import com.truelaurel.codingame.engine.GamePlayer

object GhostCellPlayer extends GamePlayer[GhostCellGameState, Vector[GhostCellAction]] {
  override def reactTo(state: GhostCellGameState): Vector[GhostCellAction] = {
    val attackPlan = FactoryAnalysis.movePlans(state)
    val attackMoves = attackPlan.map(m => {
      m.copy(to = state.transferFac(m.from, m.to))
    })

    val avoidBomb = attackMoves
      .filter(move => !state.bombs.filter(_.owner == 1).exists(b => b.to == move.to && b.explosion == state.dist(move.from, move.to)))
      .filter(move => !state.bombs.filter(_.owner == -1).exists(b => {
        val dist = state.directDist(b.from, move.to)
        val travelled = state.turn - b.birth
        val arrival = dist - travelled
        state.factories(move.to).production > 0 && arrival == state.dist(move.from, move.to) + 1
      }))

    val increasable = if (FactoryAnalysis.noIncrease(state)) {
      Vector.empty
    } else {
      state.myFacs
        .filter(fac => fac.production < 3)
        .filter(fac => FactoryAnalysis.moveAvailable(fac, state) >= 10)
        .filter(fac => (fac.cyborgs - avoidBomb.filter(_.from == fac.id).map(_.cyborgs).sum) >= 10)
    }

    withBombPlan(state, avoidBomb) ++ increasable.map(fac => IncreaseAction(fac.id))
  }

  private def withBombPlan(state: GhostCellGameState, moves: Vector[MoveAction]) = {
    val nextTroops = state.troops ++ FactoryAnalysis.movesToTroops(moves, state)
    moves ++ bombPlan(state, nextTroops)
  }


  private def bombPlan(state: GhostCellGameState, nextTroops: Vector[Troop]): Vector[BombAction] = {
    if (state.myFacs.isEmpty || state.otherFacs.isEmpty) Vector.empty else {
      findFront(state).map(front => {
        state.otherFacs
          .filter(fac => fac.production > 0 || fac.cyborgs > 5)
          .filter(fac => !state.bombs.filter(_.owner == 1).exists(b => b.to == fac.id))
          .map(of => FactoryTimeline.finalState(of, nextTroops, state.directDist(front.id, of.id) + 1))
          .filter(fs => fs.owner == -1)
          .sortBy(fs => (state.factories(fs.id).production * -1, state.directDist(front.id, fs.id)))
          .map(fs => BombAction(front.id, fs.id))
      }).getOrElse(Vector.empty)
    }
  }

  private def findFront(state: GhostCellGameState): Option[Fac] = {
    if (state.center.mine) Some(state.center) else if (state.center.other) {
      Some(state.myFacs.minBy(fac => state.otherFacs.map(of => state.directDist(of.id, fac.id)).sum))
    } else None
  }


}