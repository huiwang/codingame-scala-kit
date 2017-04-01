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
      bombBudget = BombBudget.computeBombBudget(actions, fromState, fromState.bombBudget)
    )
  }


  def main(args: Array[String]): Unit = {
    GameSimulator.evaluateOffline(
      Vector(
        GhostCellGameState(Vector(Fac(0, 0, 0, 0, 0), Fac(1, 1, 29, 2, 0), Fac(2, -1, 29, 2, 0), Fac(3, 0, 10, 2, 0), Fac(4, 0, 10, 2, 0), Fac(5, 0, 7, 2, 0), Fac(6, 0, 7, 2, 0), Fac(7, 0, 5, 1, 0), Fac(8, 0, 5, 1, 0), Fac(9, 0, 6, 2, 0), Fac(10, 0, 6, 2, 0)), Vector(), Vector(), 1, Map(1 -> 2, -1 -> 2), GhostGraph(11, Vector(Edge(0, 1, 4), Edge(0, 2, 4), Edge(0, 3, 1), Edge(0, 4, 1), Edge(0, 5, 6), Edge(0, 6, 6), Edge(0, 7, 1), Edge(0, 8, 1), Edge(0, 9, 7), Edge(0, 10, 7), Edge(1, 2, 9), Edge(1, 3, 2), Edge(1, 4, 6), Edge(1, 5, 2), Edge(1, 6, 11), Edge(1, 7, 3), Edge(1, 8, 5), Edge(1, 9, 2), Edge(1, 10, 12), Edge(2, 3, 6), Edge(2, 4, 2), Edge(2, 5, 11), Edge(2, 6, 2), Edge(2, 7, 5), Edge(2, 8, 3), Edge(2, 9, 12), Edge(2, 10, 2), Edge(3, 4, 4), Edge(3, 5, 3), Edge(3, 6, 9), Edge(3, 7, 3), Edge(3, 8, 2), Edge(3, 9, 5), Edge(3, 10, 10), Edge(4, 5, 9), Edge(4, 6, 3), Edge(4, 7, 2), Edge(4, 8, 3), Edge(4, 9, 10), Edge(4, 10, 5), Edge(5, 6, 13), Edge(5, 7, 6), Edge(5, 8, 6), Edge(5, 9, 1), Edge(5, 10, 14), Edge(6, 7, 6), Edge(6, 8, 6), Edge(6, 9, 14), Edge(6, 10, 1), Edge(7, 8, 4), Edge(7, 9, 7), Edge(7, 10, 8), Edge(8, 9, 8), Edge(8, 10, 7), Edge(9, 10, 16)))),
        GhostCellGameState(Vector(Fac(0, 0, 0, 0, 0), Fac(1, 1, 26, 0, 0), Fac(2, -1, 26, 0, 0), Fac(3, 0, 5, 3, 0), Fac(4, 0, 5, 3, 0), Fac(5, 0, 0, 0, 0), Fac(6, 0, 0, 0, 0), Fac(7, 0, 5, 1, 0), Fac(8, 0, 5, 1, 0), Fac(9, 0, 0, 0, 0), Fac(10, 0, 0, 0, 0)), Vector(), Vector(), 1, Map(1 -> 2, -1 -> 2), GhostGraph(11, Vector(Edge(0, 1, 6), Edge(0, 2, 6), Edge(0, 3, 2), Edge(0, 4, 2), Edge(0, 5, 2), Edge(0, 6, 2), Edge(0, 7, 7), Edge(0, 8, 7), Edge(0, 9, 3), Edge(0, 10, 3), Edge(1, 2, 13), Edge(1, 3, 6), Edge(1, 4, 7), Edge(1, 5, 3), Edge(1, 6, 9), Edge(1, 7, 1), Edge(1, 8, 14), Edge(1, 9, 2), Edge(1, 10, 10), Edge(2, 3, 7), Edge(2, 4, 6), Edge(2, 5, 9), Edge(2, 6, 3), Edge(2, 7, 14), Edge(2, 8, 1), Edge(2, 9, 10), Edge(2, 10, 2), Edge(3, 4, 5), Edge(3, 5, 2), Edge(3, 6, 5), Edge(3, 7, 7), Edge(3, 8, 9), Edge(3, 9, 5), Edge(3, 10, 3), Edge(4, 5, 5), Edge(4, 6, 2), Edge(4, 7, 9), Edge(4, 8, 7), Edge(4, 9, 3), Edge(4, 10, 5), Edge(5, 6, 6), Edge(5, 7, 4), Edge(5, 8, 11), Edge(5, 9, 2), Edge(5, 10, 6), Edge(6, 7, 11), Edge(6, 8, 4), Edge(6, 9, 6), Edge(6, 10, 2), Edge(7, 8, 16), Edge(7, 9, 4), Edge(7, 10, 11), Edge(8, 9, 11), Edge(8, 10, 4), Edge(9, 10, 7)))),
        GhostCellGameState(Vector(Fac(0, 0, 0, 0, 0), Fac(1, 1, 27, 2, 0), Fac(2, -1, 27, 2, 0), Fac(3, 0, 11, 3, 0), Fac(4, 0, 11, 3, 0), Fac(5, 0, 3, 1, 0), Fac(6, 0, 3, 1, 0)), Vector(), Vector(), 1, Map(1 -> 2, -1 -> 2), GhostGraph(7, Vector(Edge(0, 1, 7), Edge(0, 2, 7), Edge(0, 3, 4), Edge(0, 4, 4), Edge(0, 5, 5), Edge(0, 6, 5), Edge(1, 2, 15), Edge(1, 3, 1), Edge(1, 4, 13), Edge(1, 5, 3), Edge(1, 6, 13), Edge(2, 3, 13), Edge(2, 4, 1), Edge(2, 5, 13), Edge(2, 6, 3), Edge(3, 4, 10), Edge(3, 5, 1), Edge(3, 6, 11), Edge(4, 5, 11), Edge(4, 6, 1), Edge(5, 6, 11)))),
        GhostCellGameState(Vector(Fac(0, 0, 0, 0, 0), Fac(1, 1, 16, 1, 0), Fac(2, -1, 16, 1, 0), Fac(3, 0, 6, 2, 0), Fac(4, 0, 6, 2, 0), Fac(5, 0, 1, 1, 0), Fac(6, 0, 1, 1, 0), Fac(7, 0, 7, 3, 0), Fac(8, 0, 7, 3, 0)), Vector(), Vector(), 1, Map(1 -> 2, -1 -> 2), GhostGraph(9, Vector(Edge(0, 1, 3), Edge(0, 2, 3), Edge(0, 3, 3), Edge(0, 4, 3), Edge(0, 5, 7), Edge(0, 6, 7), Edge(0, 7, 6), Edge(0, 8, 6), Edge(1, 2, 8), Edge(1, 3, 2), Edge(1, 4, 7), Edge(1, 5, 4), Edge(1, 6, 11), Edge(1, 7, 1), Edge(1, 8, 11), Edge(2, 3, 7), Edge(2, 4, 2), Edge(2, 5, 11), Edge(2, 6, 4), Edge(2, 7, 11), Edge(2, 8, 1), Edge(3, 4, 8), Edge(3, 5, 2), Edge(3, 6, 12), Edge(3, 7, 4), Edge(3, 8, 10), Edge(4, 5, 12), Edge(4, 6, 2), Edge(4, 7, 10), Edge(4, 8, 4), Edge(5, 6, 15), Edge(5, 7, 4), Edge(5, 8, 13), Edge(6, 7, 13), Edge(6, 8, 4), Edge(7, 8, 14)))),
        GhostCellGameState(Vector(Fac(0, 0, 0, 0, 0), Fac(1, 1, 25, 2, 0), Fac(2, -1, 25, 2, 0), Fac(3, 0, 4, 3, 0), Fac(4, 0, 4, 3, 0), Fac(5, 0, 0, 3, 0), Fac(6, 0, 0, 3, 0), Fac(7, 0, 0, 0, 0), Fac(8, 0, 0, 0, 0)), Vector(), Vector(), 1, Map(1 -> 2, -1 -> 2), GhostGraph(9, Vector(Edge(0, 1, 2), Edge(0, 2, 2), Edge(0, 3, 5), Edge(0, 4, 5), Edge(0, 5, 2), Edge(0, 6, 2), Edge(0, 7, 5), Edge(0, 8, 5), Edge(1, 2, 6), Edge(1, 3, 1), Edge(1, 4, 9), Edge(1, 5, 3), Edge(1, 6, 4), Edge(1, 7, 2), Edge(1, 8, 8), Edge(2, 3, 9), Edge(2, 4, 1), Edge(2, 5, 4), Edge(2, 6, 3), Edge(2, 7, 8), Edge(2, 8, 2), Edge(3, 4, 12), Edge(3, 5, 4), Edge(3, 6, 7), Edge(3, 7, 1), Edge(3, 8, 11), Edge(4, 5, 7), Edge(4, 6, 4), Edge(4, 7, 11), Edge(4, 8, 1), Edge(5, 6, 6), Edge(5, 7, 2), Edge(5, 8, 8), Edge(6, 7, 8), Edge(6, 8, 2), Edge(7, 8, 11)))),
        GhostCellGameState(Vector(Fac(0, 0, 0, 0, 0), Fac(1, 1, 16, 3, 0), Fac(2, -1, 16, 3, 0), Fac(3, 0, 9, 3, 0), Fac(4, 0, 9, 3, 0), Fac(5, 0, 3, 3, 0), Fac(6, 0, 3, 3, 0), Fac(7, 0, 5, 1, 0), Fac(8, 0, 5, 1, 0), Fac(9, 0, 5, 3, 0), Fac(10, 0, 5, 3, 0)), Vector(), Vector(), 1, Map(1 -> 2, -1 -> 2), GhostGraph(11, Vector(Edge(0, 1, 6), Edge(0, 2, 6), Edge(0, 3, 1), Edge(0, 4, 1), Edge(0, 5, 6), Edge(0, 6, 6), Edge(0, 7, 2), Edge(0, 8, 2), Edge(0, 9, 4), Edge(0, 10, 4), Edge(1, 2, 13), Edge(1, 3, 5), Edge(1, 4, 7), Edge(1, 5, 2), Edge(1, 6, 13), Edge(1, 7, 3), Edge(1, 8, 9), Edge(1, 9, 4), Edge(1, 10, 10), Edge(2, 3, 7), Edge(2, 4, 5), Edge(2, 5, 13), Edge(2, 6, 2), Edge(2, 7, 9), Edge(2, 8, 3), Edge(2, 9, 10), Edge(2, 10, 4), Edge(3, 4, 3), Edge(3, 5, 5), Edge(3, 6, 8), Edge(3, 7, 3), Edge(3, 8, 3), Edge(3, 9, 2), Edge(3, 10, 6), Edge(4, 5, 8), Edge(4, 6, 5), Edge(4, 7, 3), Edge(4, 8, 3), Edge(4, 9, 6), Edge(4, 10, 2), Edge(5, 6, 14), Edge(5, 7, 5), Edge(5, 8, 9), Edge(5, 9, 1), Edge(5, 10, 12), Edge(6, 7, 9), Edge(6, 8, 5), Edge(6, 9, 12), Edge(6, 10, 1), Edge(7, 8, 6), Edge(7, 9, 4), Edge(7, 10, 6), Edge(8, 9, 6), Edge(8, 10, 4), Edge(9, 10, 10)))),
        GhostCellGameState(Vector(Fac(0, 0, 0, 0, 0), Fac(1, 1, 25, 0, 0), Fac(2, -1, 25, 0, 0), Fac(3, 0, 0, 0, 0), Fac(4, 0, 0, 0, 0), Fac(5, 0, 13, 3, 0), Fac(6, 0, 13, 3, 0), Fac(7, 0, 2, 1, 0), Fac(8, 0, 2, 1, 0), Fac(9, 0, 1, 2, 0), Fac(10, 0, 1, 2, 0), Fac(11, 0, 8, 2, 0), Fac(12, 0, 8, 2, 0)), Vector(), Vector(), 1, Map(1 -> 2, -1 -> 2), GhostGraph(13, Vector(Edge(0, 1, 2), Edge(0, 2, 2), Edge(0, 3, 4), Edge(0, 4, 4), Edge(0, 5, 2), Edge(0, 6, 2), Edge(0, 7, 7), Edge(0, 8, 7), Edge(0, 9, 3), Edge(0, 10, 3), Edge(0, 11, 5), Edge(0, 12, 5), Edge(1, 2, 5), Edge(1, 3, 1), Edge(1, 4, 7), Edge(1, 5, 1), Edge(1, 6, 5), Edge(1, 7, 4), Edge(1, 8, 10), Edge(1, 9, 2), Edge(1, 10, 5), Edge(1, 11, 2), Edge(1, 12, 8), Edge(2, 3, 7), Edge(2, 4, 1), Edge(2, 5, 5), Edge(2, 6, 1), Edge(2, 7, 10), Edge(2, 8, 4), Edge(2, 9, 5), Edge(2, 10, 2), Edge(2, 11, 8), Edge(2, 12, 2), Edge(3, 4, 9), Edge(3, 5, 3), Edge(3, 6, 7), Edge(3, 7, 2), Edge(3, 8, 12), Edge(3, 9, 2), Edge(3, 10, 8), Edge(3, 11, 1), Edge(3, 12, 10), Edge(4, 5, 7), Edge(4, 6, 3), Edge(4, 7, 12), Edge(4, 8, 2), Edge(4, 9, 8), Edge(4, 10, 2), Edge(4, 11, 10), Edge(4, 12, 1), Edge(5, 6, 6), Edge(5, 7, 6), Edge(5, 8, 9), Edge(5, 9, 4), Edge(5, 10, 4), Edge(5, 11, 2), Edge(5, 12, 9), Edge(6, 7, 9), Edge(6, 8, 6), Edge(6, 9, 4), Edge(6, 10, 4), Edge(6, 11, 9), Edge(6, 12, 2), Edge(7, 8, 15), Edge(7, 9, 3), Edge(7, 10, 11), Edge(7, 11, 4), Edge(7, 12, 13), Edge(8, 9, 11), Edge(8, 10, 3), Edge(8, 11, 13), Edge(8, 12, 4), Edge(9, 10, 7), Edge(9, 11, 4), Edge(9, 12, 8), Edge(10, 11, 8), Edge(10, 12, 4), Edge(11, 12, 11)))),
        GhostCellGameState(Vector(Fac(0, 0, 0, 0, 0), Fac(1, 1, 19, 1, 0), Fac(2, -1, 19, 1, 0), Fac(3, 0, 13, 3, 0), Fac(4, 0, 13, 3, 0), Fac(5, 0, 2, 2, 0), Fac(6, 0, 2, 2, 0), Fac(7, 0, 10, 2, 0), Fac(8, 0, 10, 2, 0)), Vector(), Vector(), 1, Map(1 -> 2, -1 -> 2), GhostGraph(9, Vector(Edge(0, 1, 2), Edge(0, 2, 2), Edge(0, 3, 2), Edge(0, 4, 2), Edge(0, 5, 5), Edge(0, 6, 5), Edge(0, 7, 1), Edge(0, 8, 1), Edge(1, 2, 6), Edge(1, 3, 1), Edge(1, 4, 5), Edge(1, 5, 2), Edge(1, 6, 9), Edge(1, 7, 2), Edge(1, 8, 4), Edge(2, 3, 5), Edge(2, 4, 1), Edge(2, 5, 9), Edge(2, 6, 2), Edge(2, 7, 4), Edge(2, 8, 2), Edge(3, 4, 5), Edge(3, 5, 5), Edge(3, 6, 7), Edge(3, 7, 4), Edge(3, 8, 2), Edge(4, 5, 7), Edge(4, 6, 5), Edge(4, 7, 2), Edge(4, 8, 4), Edge(5, 6, 12), Edge(5, 7, 4), Edge(5, 8, 8), Edge(6, 7, 8), Edge(6, 8, 4), Edge(7, 8, 5)))),
        GhostCellGameState(Vector(Fac(0, 0, 0, 0, 0), Fac(1, 1, 17, 2, 0), Fac(2, -1, 17, 2, 0), Fac(3, 0, 12, 3, 0), Fac(4, 0, 12, 3, 0), Fac(5, 0, 11, 3, 0), Fac(6, 0, 11, 3, 0), Fac(7, 0, 6, 2, 0), Fac(8, 0, 6, 2, 0), Fac(9, 0, 8, 3, 0), Fac(10, 0, 8, 3, 0), Fac(11, 0, 4, 3, 0), Fac(12, 0, 4, 3, 0)), Vector(), Vector(), 1, Map(1 -> 2, -1 -> 2), GhostGraph(13, Vector(Edge(0, 1, 4), Edge(0, 2, 4), Edge(0, 3, 3), Edge(0, 4, 3), Edge(0, 5, 6), Edge(0, 6, 6), Edge(0, 7, 2), Edge(0, 8, 2), Edge(0, 9, 8), Edge(0, 10, 8), Edge(0, 11, 3), Edge(0, 12, 3), Edge(1, 2, 9), Edge(1, 3, 2), Edge(1, 4, 8), Edge(1, 5, 1), Edge(1, 6, 11), Edge(1, 7, 4), Edge(1, 8, 5), Edge(1, 9, 3), Edge(1, 10, 13), Edge(1, 11, 1), Edge(1, 12, 8), Edge(2, 3, 8), Edge(2, 4, 2), Edge(2, 5, 11), Edge(2, 6, 1), Edge(2, 7, 5), Edge(2, 8, 4), Edge(2, 9, 13), Edge(2, 10, 3), Edge(2, 11, 8), Edge(2, 12, 1), Edge(3, 4, 8), Edge(3, 5, 2), Edge(3, 6, 10), Edge(3, 7, 2), Edge(3, 8, 6), Edge(3, 9, 6), Edge(3, 10, 11), Edge(3, 11, 3), Edge(3, 12, 6), Edge(4, 5, 10), Edge(4, 6, 2), Edge(4, 7, 6), Edge(4, 8, 2), Edge(4, 9, 11), Edge(4, 10, 6), Edge(4, 11, 6), Edge(4, 12, 3), Edge(5, 6, 13), Edge(5, 7, 5), Edge(5, 8, 8), Edge(5, 9, 4), Edge(5, 10, 14), Edge(5, 11, 4), Edge(5, 12, 9), Edge(6, 7, 8), Edge(6, 8, 5), Edge(6, 9, 14), Edge(6, 10, 4), Edge(6, 11, 9), Edge(6, 12, 4), Edge(7, 8, 6), Edge(7, 9, 9), Edge(7, 10, 8), Edge(7, 11, 5), Edge(7, 12, 3), Edge(8, 9, 8), Edge(8, 10, 9), Edge(8, 11, 3), Edge(8, 12, 5), Edge(9, 10, 17), Edge(9, 11, 4), Edge(9, 12, 12), Edge(10, 11, 12), Edge(10, 12, 4), Edge(11, 12, 7)))),
        GhostCellGameState(Vector(Fac(0, 0, 0, 0, 0), Fac(1, 1, 24, 0, 0), Fac(2, -1, 24, 0, 0), Fac(3, 0, 1, 1, 0), Fac(4, 0, 1, 1, 0), Fac(5, 0, 0, 0, 0), Fac(6, 0, 0, 0, 0), Fac(7, 0, 1, 2, 0), Fac(8, 0, 1, 2, 0), Fac(9, 0, 0, 2, 0), Fac(10, 0, 0, 2, 0), Fac(11, 0, 0, 0, 0), Fac(12, 0, 0, 0, 0)), Vector(), Vector(), 1, Map(1 -> 2, -1 -> 2), GhostGraph(13, Vector(Edge(0, 1, 5), Edge(0, 2, 5), Edge(0, 3, 7), Edge(0, 4, 7), Edge(0, 5, 3), Edge(0, 6, 3), Edge(0, 7, 3), Edge(0, 8, 3), Edge(0, 9, 1), Edge(0, 10, 1), Edge(0, 11, 1), Edge(0, 12, 1), Edge(1, 2, 12), Edge(1, 3, 2), Edge(1, 4, 14), Edge(1, 5, 1), Edge(1, 6, 9), Edge(1, 7, 2), Edge(1, 8, 9), Edge(1, 9, 3), Edge(1, 10, 8), Edge(1, 11, 4), Edge(1, 12, 7), Edge(2, 3, 14), Edge(2, 4, 2), Edge(2, 5, 9), Edge(2, 6, 1), Edge(2, 7, 9), Edge(2, 8, 2), Edge(2, 9, 8), Edge(2, 10, 3), Edge(2, 11, 7), Edge(2, 12, 4), Edge(3, 4, 16), Edge(3, 5, 4), Edge(3, 6, 11), Edge(3, 7, 3), Edge(3, 8, 12), Edge(3, 9, 5), Edge(3, 10, 10), Edge(3, 11, 7), Edge(3, 12, 9), Edge(4, 5, 11), Edge(4, 6, 4), Edge(4, 7, 12), Edge(4, 8, 3), Edge(4, 9, 10), Edge(4, 10, 5), Edge(4, 11, 9), Edge(4, 12, 7), Edge(5, 6, 7), Edge(5, 7, 3), Edge(5, 8, 7), Edge(5, 9, 1), Edge(5, 10, 5), Edge(5, 11, 1), Edge(5, 12, 5), Edge(6, 7, 7), Edge(6, 8, 3), Edge(6, 9, 5), Edge(6, 10, 1), Edge(6, 11, 5), Edge(6, 12, 1), Edge(7, 8, 8), Edge(7, 9, 1), Edge(7, 10, 6), Edge(7, 11, 5), Edge(7, 12, 4), Edge(8, 9, 6), Edge(8, 10, 1), Edge(8, 11, 4), Edge(8, 12, 5), Edge(9, 10, 4), Edge(9, 11, 2), Edge(9, 12, 3), Edge(10, 11, 3), Edge(10, 12, 2), Edge(11, 12, 4)))),
        GhostCellGameState(Vector(Fac(0, 0, 0, 0, 0), Fac(1, 1, 28, 3, 0), Fac(2, -1, 28, 3, 0), Fac(3, 0, 2, 3, 0), Fac(4, 0, 2, 3, 0), Fac(5, 0, 0, 0, 0), Fac(6, 0, 0, 0, 0), Fac(7, 0, 11, 3, 0), Fac(8, 0, 11, 3, 0), Fac(9, 0, 5, 1, 0), Fac(10, 0, 5, 1, 0), Fac(11, 0, 1, 2, 0), Fac(12, 0, 1, 2, 0), Fac(13, 0, 6, 2, 0), Fac(14, 0, 6, 2, 0)), Vector(), Vector(), 1, Map(1 -> 2, -1 -> 2), GhostGraph(15, Vector(Edge(0, 1, 8), Edge(0, 2, 8), Edge(0, 3, 5), Edge(0, 4, 5), Edge(0, 5, 1), Edge(0, 6, 1), Edge(0, 7, 6), Edge(0, 8, 6), Edge(0, 9, 2), Edge(0, 10, 2), Edge(0, 11, 3), Edge(0, 12, 3), Edge(0, 13, 1), Edge(0, 14, 1), Edge(1, 2, 17), Edge(1, 3, 4), Edge(1, 4, 14), Edge(1, 5, 5), Edge(1, 6, 10), Edge(1, 7, 2), Edge(1, 8, 15), Edge(1, 9, 6), Edge(1, 10, 10), Edge(1, 11, 4), Edge(1, 12, 12), Edge(1, 13, 8), Edge(1, 14, 8), Edge(2, 3, 14), Edge(2, 4, 4), Edge(2, 5, 10), Edge(2, 6, 5), Edge(2, 7, 15), Edge(2, 8, 2), Edge(2, 9, 10), Edge(2, 10, 6), Edge(2, 11, 12), Edge(2, 12, 4), Edge(2, 13, 8), Edge(2, 14, 8), Edge(3, 4, 12), Edge(3, 5, 4), Edge(3, 6, 7), Edge(3, 7, 1), Edge(3, 8, 13), Edge(3, 9, 2), Edge(3, 10, 9), Edge(3, 11, 2), Edge(3, 12, 9), Edge(3, 13, 4), Edge(3, 14, 7), Edge(4, 5, 7), Edge(4, 6, 4), Edge(4, 7, 13), Edge(4, 8, 1), Edge(4, 9, 9), Edge(4, 10, 2), Edge(4, 11, 9), Edge(4, 12, 2), Edge(4, 13, 7), Edge(4, 14, 4), Edge(5, 6, 4), Edge(5, 7, 4), Edge(5, 8, 8), Edge(5, 9, 2), Edge(5, 10, 3), Edge(5, 11, 1), Edge(5, 12, 5), Edge(5, 13, 3), Edge(5, 14, 1), Edge(6, 7, 8), Edge(6, 8, 4), Edge(6, 9, 3), Edge(6, 10, 2), Edge(6, 11, 5), Edge(6, 12, 1), Edge(6, 13, 1), Edge(6, 14, 3), Edge(7, 8, 14), Edge(7, 9, 4), Edge(7, 10, 9), Edge(7, 11, 2), Edge(7, 12, 10), Edge(7, 13, 6), Edge(7, 14, 7), Edge(8, 9, 9), Edge(8, 10, 4), Edge(8, 11, 10), Edge(8, 12, 2), Edge(8, 13, 7), Edge(8, 14, 6), Edge(9, 10, 5), Edge(9, 11, 1), Edge(9, 12, 6), Edge(9, 13, 1), Edge(9, 14, 4), Edge(10, 11, 6), Edge(10, 12, 1), Edge(10, 13, 4), Edge(10, 14, 1), Edge(11, 12, 7), Edge(11, 13, 3), Edge(11, 14, 3), Edge(12, 13, 3), Edge(12, 14, 3), Edge(13, 14, 3)))),
        GhostCellGameState(Vector(Fac(0, 0, 0, 0, 0), Fac(1, 1, 24, 0, 0), Fac(2, -1, 24, 0, 0), Fac(3, 0, 13, 3, 0), Fac(4, 0, 13, 3, 0), Fac(5, 0, 0, 2, 0), Fac(6, 0, 0, 2, 0), Fac(7, 0, 3, 1, 0), Fac(8, 0, 3, 1, 0), Fac(9, 0, 4, 1, 0), Fac(10, 0, 4, 1, 0), Fac(11, 0, 0, 0, 0), Fac(12, 0, 0, 0, 0)), Vector(), Vector(), 1, Map(1 -> 2, -1 -> 2), GhostGraph(13, Vector(Edge(0, 1, 5), Edge(0, 2, 5), Edge(0, 3, 3), Edge(0, 4, 3), Edge(0, 5, 1), Edge(0, 6, 1), Edge(0, 7, 7), Edge(0, 8, 7), Edge(0, 9, 2), Edge(0, 10, 2), Edge(0, 11, 4), Edge(0, 12, 4), Edge(1, 2, 12), Edge(1, 3, 1), Edge(1, 4, 10), Edge(1, 5, 5), Edge(1, 6, 6), Edge(1, 7, 1), Edge(1, 8, 14), Edge(1, 9, 3), Edge(1, 10, 8), Edge(1, 11, 3), Edge(1, 12, 10), Edge(2, 3, 10), Edge(2, 4, 1), Edge(2, 5, 6), Edge(2, 6, 5), Edge(2, 7, 14), Edge(2, 8, 1), Edge(2, 9, 8), Edge(2, 10, 3), Edge(2, 11, 10), Edge(2, 12, 3), Edge(3, 4, 8), Edge(3, 5, 3), Edge(3, 6, 4), Edge(3, 7, 3), Edge(3, 8, 11), Edge(3, 9, 1), Edge(3, 10, 6), Edge(3, 11, 2), Edge(3, 12, 8), Edge(4, 5, 4), Edge(4, 6, 3), Edge(4, 7, 11), Edge(4, 8, 3), Edge(4, 9, 6), Edge(4, 10, 1), Edge(4, 11, 8), Edge(4, 12, 2), Edge(5, 6, 4), Edge(5, 7, 6), Edge(5, 8, 8), Edge(5, 9, 3), Edge(5, 10, 2), Edge(5, 11, 2), Edge(5, 12, 6), Edge(6, 7, 8), Edge(6, 8, 6), Edge(6, 9, 2), Edge(6, 10, 3), Edge(6, 11, 6), Edge(6, 12, 2), Edge(7, 8, 15), Edge(7, 9, 5), Edge(7, 10, 9), Edge(7, 11, 3), Edge(7, 12, 12), Edge(8, 9, 9), Edge(8, 10, 5), Edge(8, 11, 12), Edge(8, 12, 3), Edge(9, 10, 5), Edge(9, 11, 4), Edge(9, 12, 6), Edge(10, 11, 6), Edge(10, 12, 4), Edge(11, 12, 10)))),
        GhostCellGameState(Vector(Fac(0, 0, 0, 0, 0), Fac(1, 1, 21, 1, 0), Fac(2, -1, 21, 1, 0), Fac(3, 0, 11, 3, 0), Fac(4, 0, 11, 3, 0), Fac(5, 0, 0, 0, 0), Fac(6, 0, 0, 0, 0), Fac(7, 0, 0, 0, 0), Fac(8, 0, 0, 0, 0)), Vector(), Vector(), 1, Map(1 -> 2, -1 -> 2), GhostGraph(9, Vector(Edge(0, 1, 2), Edge(0, 2, 2), Edge(0, 3, 5), Edge(0, 4, 5), Edge(0, 5, 4), Edge(0, 6, 4), Edge(0, 7, 7), Edge(0, 8, 7), Edge(1, 2, 6), Edge(1, 3, 4), Edge(1, 4, 7), Edge(1, 5, 1), Edge(1, 6, 7), Edge(1, 7, 5), Edge(1, 8, 10), Edge(2, 3, 7), Edge(2, 4, 4), Edge(2, 5, 7), Edge(2, 6, 1), Edge(2, 7, 10), Edge(2, 8, 5), Edge(3, 4, 12), Edge(3, 5, 2), Edge(3, 6, 10), Edge(3, 7, 1), Edge(3, 8, 13), Edge(4, 5, 10), Edge(4, 6, 2), Edge(4, 7, 13), Edge(4, 8, 1), Edge(5, 6, 10), Edge(5, 7, 1), Edge(5, 8, 12), Edge(6, 7, 12), Edge(6, 8, 1), Edge(7, 8, 15))))
      ),
      GhostArena,
      Vector(head.GhostCellPlayer(1), best.GhostCellPlayer(-1)))
  }


}
