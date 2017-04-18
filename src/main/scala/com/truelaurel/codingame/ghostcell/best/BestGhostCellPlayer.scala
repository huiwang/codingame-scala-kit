package com.truelaurel.codingame.ghostcell.best

import com.truelaurel.codingame.engine.GamePlayer
import com.truelaurel.codingame.ghostcell.common._

case class BestGhostCellPlayer(me: Int) extends GamePlayer[GhostCellGameState, GhostCellAction] {

  val factoryAnalysis = BestFactoryAnalysis(me)

  override def reactTo(state: GhostCellGameState): Vector[GhostCellAction] = {
    val attackPlan = factoryAnalysis.movePlans(state)
    val attackMoves = attackPlan.map(m => {
      m.copy(to = state.transferFac(m.from, m.to))
    })

    val avoidBomb = attackMoves
      .filter(move => !state.bombs.filter(_.owner == me).exists(b => b.to == move.to && b.explosion == state.dist(move.from, move.to)))
      .filter(move => !state.bombs.filter(_.owner == -me).exists(b => {
        val dist = state.directDist(b.from, move.to)
        val travelled = state.turn - b.observed
        val arrival = dist - travelled
        state.factories(move.to).production > 0 && arrival == state.dist(move.from, move.to) + 1
      }))

    val increasable = if (factoryAnalysis.noIncrease(state)) {
      Vector.empty
    } else {
      state.factories.filter(_.owner == me)
        .filter(fac => fac.production < 3)
        .filter(fac => factoryAnalysis.moveAvailable(fac, state) >= 10)
        .filter(fac => (fac.cyborgs - avoidBomb.filter(_.from == fac.id).map(_.cyborgs).sum) >= 10)
    }

    withBombPlan(state, avoidBomb) ++ increasable.map(fac => IncreaseAction(fac.id))
  }

  private def withBombPlan(state: GhostCellGameState, moves: Vector[MoveAction]) = {
    val nextTroops = state.troops ++ factoryAnalysis.movesToTroops(moves, state)
    moves ++ bombPlan(state, nextTroops)
  }


  private def bombPlan(state: GhostCellGameState, nextTroops: Vector[Troop]): Vector[BombAction] = {
    if (!state.factories.exists(_.owner == me) || !state.factories.exists(_.owner == -me)) Vector.empty else {
      findFront(state).map(front => {
        state.factories.filter(_.owner == -me)
          .filter(fac => fac.production > 0 || fac.cyborgs > 5)
          .filter(fac => !state.bombs.filter(_.owner == me).exists(b => b.to == fac.id))
          .map(of => FactoryTimeline.finalState(of, nextTroops, state.directDist(front.id, of.id) + 1))
          .filter(fs => fs.owner == -me)
          .sortBy(fs => (state.factories(fs.id).production * -1, state.directDist(front.id, fs.id)))
          .take(state.newBombBudget(me))
          .map(fs => BombAction(front.id, fs.id))
      }).getOrElse(Vector.empty)
    }
  }

  private def findFront(state: GhostCellGameState): Option[Fac] = {
    if (state.center.owner == me) Some(state.center) else if (state.center.owner == -me) {
      Some(state.factories.filter(_.owner == me).minBy(fac => state.factories.filter(_.owner == -me).map(of => state.directDist(of.id, fac.id)).sum))
    } else None
  }


}