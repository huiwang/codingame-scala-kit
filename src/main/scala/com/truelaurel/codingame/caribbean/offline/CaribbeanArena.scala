package com.truelaurel.codingame.caribbean.offline

import com.truelaurel.codingame.caribbean.common._
import com.truelaurel.codingame.engine.{GameArena, GameResult}

/**
  * Created by hwang on 15/04/2017.
  */
object CaribbeanArena extends GameArena[CaribbeanState, CaribbeanAction] {
  override def next(state: CaribbeanState, actions: Vector[CaribbeanAction]): CaribbeanState = {
    val movedBalls = state.balls.map(b => b.copy(land = b.land - 1))
    val shipMap = state.ships.map(s => s.id -> s).toMap
    val barrelMap = state.barrels.map(b => b.id -> b).toMap
    val mineMap = state.mines.map(m => m.id -> m).toMap

    val shipsAfterDecresedRum = shipMap.mapValues(s => s.copy(rums = s.rums - 1)).filter(_._2.rums > 0)
    val actionByShip = actions.map(a => a.shipId -> a).toMap

    val shipsAfterSpeeding = shipsAfterDecresedRum.mapValues(ship => {
      actionByShip(ship.id) match {
        case Faster(_) => ship.copy(speed = 2.min(ship.speed + 1))
        case Slower(_) => ship.copy(speed = 0.max(ship.speed - 1))
        case _ => ship
      }
    }).mapValues(moveShip)

    val shipsAfterMoveActionReact = reactToShipAction(shipsAfterDecresedRum, shipsAfterSpeeding)
    val (shipsAfterMoveReact, barrelsAfterMoveReact, minesAfterMoveReact) =
      reactToShipsChange(shipsAfterMoveActionReact, barrelMap, mineMap)

    val shipsAfterRotation = shipsAfterMoveReact.mapValues(ship => {
      actionByShip(ship.id) match {
        case Port(_) => ship.copy(orientation = (ship.orientation + 1) % 6)
        case Starboard(_) => ship.copy(orientation = (ship.orientation + 5) % 6)
        case _ => ship
      }
    })

    val shipsAfterRotationActionReact = reactToShipAction(shipsAfterMoveActionReact, shipsAfterRotation)
    val (shipsAfterRotationReact, barrelsAfterRotationReact, minesAfterRotationReact) =
      reactToShipsChange(shipsAfterRotationActionReact, barrelsAfterMoveReact, minesAfterMoveReact)

    state.copy(
      ships = shipsAfterRotationReact.values.toVector,
      barrels = barrelsAfterRotationReact.values.toVector,
      mines = minesAfterRotationReact.values.toVector,
      turn = state.turn + 1
    )
  }

  def reactToShipAction(shipsBeforeAction: Map[Int, Ship],
                        shipsAfterAction: Map[Int, Ship]): Map[Int, Ship] = {
    val collisions = shipCollisions(shipsAfterAction.values.toVector)
    shipsBeforeAction.filterKeys(collisions).mapValues(_.copy(speed = 0)) ++
      shipsAfterAction.filterNot(e => collisions.contains(e._1))
  }


  def reactToShipsChange(ships: Map[Int, Ship],
                         barrels: Map[Int, Barrel],
                         mines: Map[Int, Mine]): (Map[Int, Ship], Map[Int, Barrel], Map[Int, Mine]) = {
    val shipBarrelCollision = for {
      ship <- ships.values
      barrel <- barrels.values
      if ship.cubeSet.contains(barrel.cube)
    } yield (ship, barrel)

    val increasedShips = (for {
      (ship, barrel) <- shipBarrelCollision
    } yield ship.id -> ship.copy(rums = ship.rums + barrel.rums)).toMap

    val shipsAfterShipBarrelCollision = ships ++ increasedShips

    val consumedBarrelsAfterMove = (for {
      (ship, barrel) <- shipBarrelCollision
    } yield barrel.id).toSet

    val barrlesAfterShipMove = barrels.filterNot(e => consumedBarrelsAfterMove.contains(e._1))

    val shipMineCollision = for {
      ship <- shipsAfterShipBarrelCollision.values
      mine <- mines.values
      if CaribbeanConstants.cubeToNeighbors(mine.cube).toSet.intersect(ship.cubeSet).nonEmpty
    } yield (ship, mine)

    val damagedShips = (for {
      (ship, mine) <- shipMineCollision
    } yield ship.id -> ship.copy(rums = ship.rums -
      (if (ship.cubeSet.contains(mine.cube)) CaribbeanConstants.highMineDamage else CaribbeanConstants.lowMineDamage))).toMap

    val shipsAfterShipMineCollision = shipsAfterShipBarrelCollision ++ damagedShips

    val consumedMinesAfterMove = (for {
      (ship, mine) <- shipMineCollision
    } yield mine.id).toSet

    val minesAfterShipMove = mines.filterNot(e => consumedMinesAfterMove.contains(e._1))

    (shipsAfterShipMineCollision, barrlesAfterShipMove, minesAfterShipMove)
  }

  def moveShip(ship: Ship): Ship = {
    val nextShip = next(ship)
    if (!CaribbeanConstants.cubes.contains(nextShip.center)) {
      ship.copy(speed = 0)
    } else {
      nextShip
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

  def next(ship: Ship): Ship = {
    (0 until ship.speed).foldLeft(ship)((ship, _) => ship.copy(position = ship.bow.toOffset))
  }

  def collided(one: Ship, other: Ship): Boolean = {
    one.cubeSet.intersect(other.cubeSet).nonEmpty
  }


  override def judge(state: CaribbeanState): GameResult = ???
}
