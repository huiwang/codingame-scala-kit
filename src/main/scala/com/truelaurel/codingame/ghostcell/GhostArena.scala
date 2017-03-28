package com.truelaurel.codingame.ghostcell

import com.truelaurel.codingame.engine.GameArena

/**
  * Created by hwang on 25/03/2017.
  */
object GhostArena extends GameArena[GhostCellGameState, GhostCellAction] {
  override def execute(fromState: GhostCellGameState, actions: Vector[GhostCellAction]): GhostCellGameState = {

    val nextTroops = fromState.troops.map(t => t.copy(arrival = t.arrival - 1))

    val nextBombs = fromState.bombs.map(b => b.copy(explosion = b.explosion - 1))

    val newTroops = actions.flatMap {
      case MoveAction(from, to, cyborgs) => Some(Troop(-1, fromState.fac(from).owner, from, to, cyborgs, fromState.dist(from, to)))
      case _ => None
    }

    val newBombs = actions.flatMap {
      case BombAction(from, to) => Some(Bomb(-1, fromState.fac(from).owner, from, to, fromState.directDist(from, to), fromState.turn))
      case _ => None
    }

    val afterDeparture = for {
      fac <- fromState.factories
      departure = newTroops.find(_.from == fac.id).map(_.cyborgs).getOrElse(0)
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
      newCyborgs = if (cooledDown == 0) fac.production else 0
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
    } yield if (bombed) fac.copy(again = 5, cyborgs = fac.cyborgs - 10.max((fac.cyborgs / 2).floor.toInt)) else fac

    fromState.copy(factories = afterBomb,
      troops = nextTroops.filter(_.arrival > 0) ++ newTroops,
      bombs = nextBombs.filter(_.explosion > 0) ++ newBombs,
      turn = fromState.turn + 1,
      bombBudget = computeBombBudget(actions, fromState, fromState.bombBudget)
    )
  }

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
