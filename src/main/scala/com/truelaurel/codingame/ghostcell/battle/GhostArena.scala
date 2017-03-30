package com.truelaurel.codingame.ghostcell.battle

import com.truelaurel.codingame.engine._
import com.truelaurel.codingame.ghostcell.common._
import com.truelaurel.codingame.ghostcell.{best, _}
import com.truelaurel.codingame.graph.Edge

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
      if(diff > 0) WinTech else if(diff < 0) LossTech else Draw
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
      case BombAction(from, to) => Some(Bomb(-1, fromState.fac(from).owner, from, to, fromState.directDist(from, to), fromState.turn))
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
      damage = Math.min(fac.cyborgs, Math.max(10, (fac.cyborgs / 2).floor.toInt))
    } yield if (bombed) fac.copy(again = 5, cyborgs = fac.cyborgs - damage) else fac

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

  def main(args: Array[String]): Unit = {
    val state = GhostCellGameState(Vector(Fac(0, 0, 0, 0, 0), Fac(1, 1, 27, 2, 0), Fac(2, -1, 27, 2, 0), Fac(3, 0, 11, 3, 0), Fac(4, 0, 11, 3, 0), Fac(5, 0, 3, 1, 0), Fac(6, 0, 3, 1, 0)), Vector(), Vector(), 1, Map(1 -> 2, -1 -> 2), GhostGraph(7, Vector(Edge(0, 1, 7), Edge(0, 2, 7), Edge(0, 3, 4), Edge(0, 4, 4), Edge(0, 5, 5), Edge(0, 6, 5), Edge(1, 2, 15), Edge(1, 3, 1), Edge(1, 4, 13), Edge(1, 5, 3), Edge(1, 6, 13), Edge(2, 3, 13), Edge(2, 4, 1), Edge(2, 5, 13), Edge(2, 6, 3), Edge(3, 4, 10), Edge(3, 5, 1), Edge(3, 6, 11), Edge(4, 5, 11), Edge(4, 6, 1), Edge(5, 6, 11))))
    println(GameSimulator.play(state, GhostArena, Vector(head.GhostCellPlayer(1), best.GhostCellPlayer(-1))))
  }


}
