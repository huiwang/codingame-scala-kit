package com.truelaurel.codingame.caribbean.offline

import com.truelaurel.codingame.caribbean.common._
import com.truelaurel.codingame.caribbean.online.CaribbeanController
import com.truelaurel.codingame.engine._
import com.truelaurel.codingame.hexagons.Cube

/**
  * Created by hwang on 15/04/2017.
  */
object CaribbeanArena extends GameArena[CaribbeanState, CaribbeanAction] {
  override def next(state: CaribbeanState, actions: Vector[CaribbeanAction]): CaribbeanState = {
    val shipMap: Map[Int, Ship] = state.ships.map(s => s.id -> s).toMap
    val barrelMap: Map[Int, Barrel] = state.barrels.map(b => b.id -> b).toMap
    val mineMap: Map[Int, Mine] = state.mines.map(m => m.id -> m).toMap

    val movedBalls = state.balls.map(b => b.copy(land = b.land - 1))

    val shipsAfterDecreasedRum = shipMap.mapValues(s => s.copy(rums = s.rums - 1)).filter(_._2.rums > 0)

    val actionByShip = actions.map(a => a.shipId -> a).toMap

    val firedBalls = actionByShip.values.flatMap {
      case Fire(shipId, target) =>
        val ship = shipMap(shipId)
        val targetCube = CaribbeanContext.toCube(target)
        val distance = ship.bow.distanceTo(targetCube)
        val lastFire = state.context.lastFire.getOrElse(shipId, -1)
        if (CaribbeanContext.cubes.contains(targetCube) &&
          distance <= CaribbeanContext.fireMaxDistance &&
          state.turn - lastFire >= 2) {
          val travelTime = (1 + (distance / 3.0).round).toInt
          Some(Ball(-1, target, ship.owner, travelTime))
        } else None
      case _ => None
    }

    val shipsAfterSpeeding = shipsAfterDecreasedRum.mapValues(ship => {
      actionByShip(ship.id) match {
        case Faster(_) => ship.copy(speed = 2.min(ship.speed + 1))
        case Slower(_) => ship.copy(speed = 0.max(ship.speed - 1))
        case _ => ship
      }
    })

    val shipsAfterFirstMove = shipsAfterSpeeding.mapValues(s => moveShip(s, 1))

    val (shipsAfterFirstMoveImpact, barrelsAfterFirstMoveImpact, minesAfterFirstMoveImpact) =
      reactToShips(shipsAfterDecreasedRum, shipsAfterFirstMove, barrelMap, mineMap)

    val shipsAfterSecondMove = shipsAfterFirstMoveImpact.mapValues(s => moveShip(s, 2))

    val (shipsAfterSecondMoveImpact, barrelsAfterSecondMoveImpact, minesAfterSecondMoveImpact) =
      reactToShips(shipsAfterFirstMoveImpact, shipsAfterSecondMove,
        barrelsAfterFirstMoveImpact, minesAfterFirstMoveImpact)

    val shipsAfterRotation = shipsAfterSecondMoveImpact.mapValues(ship => {
      actionByShip(ship.id) match {
        case Port(_) => ship.copy(orientation = (ship.orientation + 1) % 6)
        case Starboard(_) => ship.copy(orientation = (ship.orientation + 5) % 6)
        case _ => ship
      }
    })

    val (shipsAfterRotationReact, barrelsAfterRotationReact, minesAfterRotationReact) =
      reactToShips(shipsAfterSecondMoveImpact, shipsAfterRotation,
        barrelsAfterSecondMoveImpact, minesAfterSecondMoveImpact)


    val (ballExplosions, remainingBalls) = movedBalls.partition(_.land == 0)

    val shipsAfterExplosion = shipsAfterRotationReact.mapValues(ship => {
      val dmgOnBow = ballExplosions.count(b => b.cube == ship.bow) * CaribbeanContext.lowBallDamage
      val dmgOnStern = ballExplosions.count(b => b.cube == ship.stern) * CaribbeanContext.lowBallDamage
      val dmgOnCenter = ballExplosions.count(b => b.cube == ship.center) * CaribbeanContext.highBallDamage
      ship.copy(rums = ship.rums - dmgOnBow - dmgOnStern - dmgOnCenter)
    })

    state.copy(
      context = CaribbeanController.nextContext(state.context, state, actions),
      ships = state.ships.flatMap(s => shipsAfterExplosion.get(s.id)).filter(_.rums > 0),
      barrels = state.barrels.flatMap(b => barrelsAfterRotationReact.get(b.id)),
      mines = state.mines.flatMap(m => minesAfterRotationReact.get(m.id)),
      balls = remainingBalls ++ firedBalls,
      turn = state.turn + 1
    )
  }

  def reactToShips(shipsBeforeAction: Map[Int, Ship],
                   shipsAfterAction: Map[Int, Ship],
                   barrels: Map[Int, Barrel],
                   mines: Map[Int, Mine]
                  ): (Map[Int, Ship], Map[Int, Barrel], Map[Int, Mine]) = {
    val shipsAfterActionReact = reactToShipsAction(shipsBeforeAction, shipsAfterAction)
    reactShipsImpacts(shipsAfterActionReact, barrels, mines)
  }


  def reactToShipsAction(shipsBeforeAction: Map[Int, Ship],
                         shipsAfterAction: Map[Int, Ship]): Map[Int, Ship] = {

    val collisions = shipCollisions(shipsAfterAction.values.toVector)
    shipsAfterAction.mapValues(ship => {
      if (collisions.contains(ship.id)) shipsBeforeAction(ship.id).copy(speed = 0) else ship
    })
  }

  def reactShipsImpacts(ships: Map[Int, Ship],
                        barrels: Map[Int, Barrel],
                        mines: Map[Int, Mine]
                       ): (Map[Int, Ship], Map[Int, Barrel], Map[Int, Mine]) = {
    val cubeToBarrel: Map[Cube, Barrel] = barrels.values.map(b => b.cube -> b).toMap
    val cubeToMine: Map[Cube, Mine] = mines.values.map(b => b.cube -> b).toMap

    val shipBarrelCollision: Iterable[(Ship, Barrel)] = findShipBarrelCollision(ships, cubeToBarrel)

    val shipsAfterShipBarrelCollision = shipBarrelCollision.foldLeft(ships) {
      case (updatedShips, (ship, barrel)) => updatedShips.updated(ship.id, ship.copy(rums = 100.min(ship.rums + barrel.rums)))
    }

    val barrelsAfterShipMove = shipBarrelCollision.foldLeft(barrels) {
      case (remaining, (_, barrel)) => remaining - barrel.id
    }

    val shipMineCollision: Iterable[(Mine, Ship)] = findShipMineCollisions(ships, cubeToMine)

    val shipsAfterShipMineCollision = shipMineCollision.foldLeft(shipsAfterShipBarrelCollision) {
      case (updatedShips, (_, ship)) =>
        updatedShips.updated(ship.id, ship.copy(rums = ship.rums - CaribbeanContext.highMineDamage))
    }

    val minesAfterShipMove = shipMineCollision.foldLeft(mines) {
      case (remaining, (mine, _)) => remaining - mine.id
    }

    (shipsAfterShipMineCollision, barrelsAfterShipMove, minesAfterShipMove)
  }

  private def findShipMineCollisions(ships: Map[Int, Ship], cubeToMine: Map[Cube, Mine]) = {
    val shipMineCollision = for {
      ship <- ships.values
      shipZone <- ship.zone
      mine <- cubeToMine.get(shipZone)
    } yield (mine, ship)
    shipMineCollision
  }

  private def findShipBarrelCollision(ships: Map[Int, Ship], cubeToBarrel: Map[Cube, Barrel]) = {
    for {
      ship <- ships.values
      shipCube <- ship.zone
      barrel <- cubeToBarrel.get(shipCube)
    } yield (ship, barrel)
  }

  def moveShip(ship: Ship, speed: Int): Ship = {
    if (ship.speed >= speed) {
      if (!CaribbeanContext.cubes.contains(ship.bow)) {
        ship.copy(speed = 0)
      } else {
        ship.copy(position = ship.bow.toOffset)
      }
    } else {
      ship
    }
  }

  def checkCollision(shipOption: Option[Ship], collisions: Set[Ship]): Option[Ship] = {
    shipOption.flatMap(ship => if (collisions.contains(ship)) None else Some(ship))
  }

  def shipCollisions(ships: Vector[Ship]): Set[Int] = {
    ships.combinations(2).foldLeft(Set[Int]()) {
      case (collisions, shipPair) =>
        if (collided(shipPair.head, shipPair.last)) {
          collisions + shipPair.head.id + shipPair.last.id
        } else collisions
    }
  }

  def collided(one: Ship, other: Ship): Boolean = {
    one.zone.intersect(other.zone).nonEmpty
  }


  override def judge(state: CaribbeanState): GameResult = {
    val myShips = state.shipsOf(CaribbeanContext.me)
    val otherShips = state.shipsOf(CaribbeanContext.other)
    if (myShips.isEmpty && otherShips.isEmpty) {
      Draw
    } else if (myShips.isEmpty) {
      LossKO
    } else if (otherShips.isEmpty) {
      WinKO
    } else {
      val myTotalRums = myShips.map(_.rums).sum
      val otherTotalRums = otherShips.map(_.rums).sum
      if (myTotalRums > otherTotalRums) {
        WinTech
      } else if (myTotalRums == otherTotalRums) {
        LossTech
      } else {
        Draw
      }
    }
  }

}
