package com.truelaurel.codingame.ghostcell.arena

import com.truelaurel.codingame.engine._
import com.truelaurel.codingame.ghostcell.common._

/**
  * Created by hwang on 25/03/2017.
  */
object GhostArena extends GameArena[GhostCellGameState, GhostCellAction] {
  override def judge(state: GhostCellGameState): GameResult = {
    if (noFacNoTroop(state, 1)) {
      LossKO
    } else if (noFacNoTroop(state, -1)) {
      WinKO
    } else {
      val diff = cyborgs(state, 1) - cyborgs(state, -1)
      if (diff > 0) {
        WinTech
      } else if (diff < 0) {
        LossTech
      } else {
        Draw
      }
    }
  }

  def noFacNoTroop(state: GhostCellGameState, playerId: Int): Boolean = {
    !state.factories.exists(_.owner == playerId) && !state.troops.exists(_.owner == playerId)
  }

  def cyborgs(state: GhostCellGameState, playerId: Int): Int = {
    state.factories.filter(_.owner == playerId).map(_.cyborgs).sum + state.troops.filter(_.owner == playerId).map(_.cyborgs).sum
  }


  override def next(fromState: GhostCellGameState, actions: Vector[GhostCellAction]): GhostCellGameState = {

    val nextTroops = fromState.troops.map(t => t.copy(arrival = t.arrival - 1))

    val nextBombs = fromState.bombs.map(b => b.copy(explosion = b.explosion - 1))

    val newTroops = actions.flatMap {
      case MoveAction(from, to, cyborgs) => Some(Troop(-1, fromState.fac(from).owner, from, to, cyborgs, fromState.dist(from, to)))
      case _ => None
    }

    val newBombs = actions.flatMap {
      case BombAction(from, to) => Some(Bomb(-1, fromState.fac(from).owner, from, to, fromState.directDist(from, to), fromState.turn + 1))
      case _ => None
    }

    val departures = newTroops.groupBy(_.from).mapValues(troops => troops.map(_.cyborgs).sum)

    val afterDeparture = for {
      fac <- fromState.factories
      departure = departures.getOrElse(fac.id, 0)
    } yield fac.copy(cyborgs = fac.cyborgs - departure)

    val afterInc = for {
      fac <- afterDeparture
      shouldInc = actions.exists {
        case IncreaseAction(facId) => facId == fac.id
        case _ => false
      }
    } yield if (shouldInc) fac.copy(cyborgs = fac.cyborgs - 10, production = fac.production + 1) else fac

    val afterProduction = for {
      fac <- afterInc
      cooledDown = 0.max(fac.again - 1)
      newCyborgs = if (cooledDown == 0 && fac.owner != 0) fac.production else 0
    } yield fac.copy(cyborgs = fac.cyborgs + newCyborgs, again = cooledDown)

    val troopsByDest = nextTroops.filter(_.arrival == 0).groupBy(_.to)

    val afterFight = afterProduction.map(fac => {
      val arrived = troopsByDest.getOrElse(fac.id, Vector.empty).map(t => t.owner * t.cyborgs).sum
      fac.owner match {
        case 0 =>
          if (arrived.abs > fac.cyborgs) {
            fac.copy(owner = arrived / arrived.abs, cyborgs = arrived.abs - fac.cyborgs)
          } else {
            fac.copy(cyborgs = fac.cyborgs - arrived.abs)
          }
        case 1 =>
          if (arrived + fac.cyborgs < 0) {
            fac.copy(owner = -1, cyborgs = arrived.abs - fac.cyborgs)
          } else {
            fac.copy(owner = 1, cyborgs = arrived + fac.cyborgs)
          }
        case -1 =>
          if (arrived - fac.cyborgs > 0) {
            fac.copy(owner = 1, cyborgs = arrived - fac.cyborgs)
          } else {
            fac.copy(owner = -1, cyborgs = fac.cyborgs - arrived)
          }
      }
    })

    val afterBomb = for {
      fac <- afterFight
      bombed = nextBombs.exists(b => b.explosion == 0 && b.to == fac.id)
      damage = Math.min(fac.cyborgs, Math.max(10, (fac.cyborgs / 2).floor.toInt))
    } yield if (bombed) fac.copy(again = 5, cyborgs = fac.cyborgs - damage) else fac

    val troops = nextTroops.filter(_.arrival > 0) ++ newTroops
    val groupedTroops = troops.groupBy(troop => (troop.owner, troop.from, troop.to, troop.arrival)).map {
      case ((owner, from, to, arrival), sameTroops) => (owner, from, to, arrival) -> Troop(-1, owner, from, to, sameTroops.map(_.cyborgs).sum, arrival)
    }.values.toVector
    val bombs = nextBombs.filter(_.explosion > 0) ++ newBombs
    fromState.copy(factories = afterBomb,
      troops = groupedTroops,
      bombs = bombs,
      turn = fromState.turn + 1,
      bombBudget = BombBudget.computeBombBudget(actions, fromState, fromState.bombBudget)
    )
  }

}
