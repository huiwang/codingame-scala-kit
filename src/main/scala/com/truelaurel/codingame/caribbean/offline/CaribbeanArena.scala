package com.truelaurel.codingame.caribbean.offline

import java.util.concurrent.atomic.AtomicInteger

import com.truelaurel.codingame.caribbean.common._
import com.truelaurel.codingame.caribbean.online.CaribbeanController
import com.truelaurel.codingame.engine._
import com.truelaurel.codingame.hexagons.Cube

/**
  * Created by hwang on 15/04/2017.
  */
object CaribbeanArena extends GameArena[CaribbeanState, CaribbeanAction] {

  val counter = new AtomicInteger(-1)

  override def next(state: CaribbeanState, actions: Vector[CaribbeanAction]): CaribbeanState = {

    val movedBalls = state.balls.mapValues(b => b.copy(land = b.land - 1))

    val shipsAfterDecreasedRum = state.ships.mapValues(s => s.copy(rums = s.rums - 1)).filter(_._2.rums > 0)

    val actionByShip = actions.map(a => a.shipId -> a).toMap

    val firedBalls = actionByShip.filter(_._2.isInstanceOf[Fire]).map {
      case (_, Fire(shipId, target)) =>
        val ship = state.ships(shipId)
        val targetCube = CaribbeanContext.toCube(target)
        val distance = ship.bow.distanceTo(targetCube)
        val travelTime = CollisionAnalysis.travelTime(distance)
        val id = counter.getAndDecrement()
        id -> Ball(id, target, ship.owner, travelTime)
      case _ => throw new IllegalStateException("Already filtered")
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
      reactToShips(shipsAfterDecreasedRum, shipsAfterFirstMove, state.barrels, state.mines)

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


    val (ballExplosions, remainingBalls) = movedBalls.partition(_._2.land == 0)

    val (mineExplosions, remainingMines) = minesAfterRotationReact.partition(e => ballExplosions.exists(_._2.cube == e._2.cube))

    val shipsAfterExplosion = shipsAfterRotationReact.mapValues(ship => {
      val dmgOnBow = ballExplosions.count(b => b._2.cube == ship.bow) * CaribbeanContext.lowBallDamage
      val dmgOnStern = ballExplosions.count(b => b._2.cube == ship.stern) * CaribbeanContext.lowBallDamage
      val dmgOnCenter = ballExplosions.count(b => b._2.cube == ship.center) * CaribbeanContext.highBallDamage
      val dmgFromMine = mineExplosions.count(m => m._2.cube.distanceTo(ship.center) == 1) * CaribbeanContext.lowMineDamage
      ship.copy(rums = ship.rums - dmgOnBow - dmgOnStern - dmgOnCenter - dmgFromMine)
    })

    state.copy(
      context = CaribbeanController.nextContext(state.context, state, actions),
      ships = shipsAfterExplosion.filter(_._2.rums > 0),
      barrels = barrelsAfterRotationReact,
      mines = remainingMines,
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
      case (updatedShips, (ship, barrel)) => updatedShips.updated(ship.id, ship.copy(rums =
        CaribbeanContext.maxRums.min(ship.rums + barrel.rums)))
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
      shipZone <- ship.bowAndStern
      mine <- cubeToMine.get(shipZone)
    } yield (mine, ship)
    shipMineCollision
  }

  private def findShipBarrelCollision(ships: Map[Int, Ship], cubeToBarrel: Map[Cube, Barrel]) = {
    for {
      ship <- ships.values
      shipCube <- ship.bowAndStern
      barrel <- cubeToBarrel.get(shipCube)
    } yield (ship, barrel)
  }

  def moveShip(ship: Ship, speed: Int): Ship = {
    if (ship.speed >= speed) {
      if (!CaribbeanContext.inside(ship.bow.toOffset)) {
        ship.copy(speed = 0)
      } else {
        ship.copy(position = ship.bow.toOffset)
      }
    } else {
      ship
    }
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
    one.center.distanceTo(other.center) < 3 && one.zone.exists(other.zone.contains)
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
