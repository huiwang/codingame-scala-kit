package com.truelaurel.codingame.caribbean.offline

import com.truelaurel.codingame.caribbean.common._
import com.truelaurel.codingame.engine.{GameArena, GameResult}
import com.truelaurel.codingame.hexagons.Cube

/**
  * Created by hwang on 15/04/2017.
  */
object CaribbeanArena extends GameArena[CaribbeanState, CaribbeanAction] {
  override def next(state: CaribbeanState, actions: Vector[CaribbeanAction]): CaribbeanState = {
    val movedBalls = state.balls.map(b => b.copy(land = b.land - 1))

    val shipsAfterDecreasedRum = state.shipMap.mapValues(s => s.copy(rums = s.rums - 1)).filter(_._2.rums > 0)
    val actionByShip = actions.map(a => a.shipId -> a).toMap

    val shipsAfterSpeeding = shipsAfterDecreasedRum.mapValues(ship => {
      actionByShip(ship.id) match {
        case Faster(_) => ship.copy(speed = 2.min(ship.speed + 1))
        case Slower(_) => ship.copy(speed = 0.max(ship.speed - 1))
        case _ => ship
      }
    })

    val shipsAfterFirstMove = shipsAfterSpeeding.mapValues(s => moveShip(s, 1))

    val (shipsAfterFirstMoveImpact, barrelsAfterFirstMoveImpact, minesAfterFirstMoveImpact) =
      reactToShips(shipsAfterDecreasedRum, shipsAfterFirstMove, state.barrelMap, state.mineMap)

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
      ships = state.ships.flatMap(s => shipsAfterExplosion.get(s.id)).filter(_.rums > 0),
      barrels = state.barrels.flatMap(b => barrelsAfterRotationReact.get(b.id)),
      mines = state.mines.flatMap(m => minesAfterRotationReact.get(m.id)),
      balls = remainingBalls,
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
    shipsBeforeAction.filterKeys(collisions).mapValues(_.copy(speed = 0)) ++
      shipsAfterAction.filterNot(e => collisions.contains(e._1))

  }


  def reactShipsImpacts(ships: Map[Int, Ship],
                        barrels: Map[Int, Barrel],
                        mines: Map[Int, Mine]
                       ): (Map[Int, Ship], Map[Int, Barrel], Map[Int, Mine]) = {
    val cubeToBarrel: Map[Cube, Barrel] = barrels.values.map(b => b.cube -> b).toMap
    val cubeToMine: Map[Cube, Mine] = mines.values.map(b => b.cube -> b).toMap

    val shipBarrelCollision = for {
      ship <- ships.values
      barrel <- CaribbeanContext.shipZone(ship).flatMap(cube => cubeToBarrel.get(cube))
    } yield (ship, barrel)

    val shipsAfterShipBarrelCollision = shipBarrelCollision.foldLeft(ships) {
      case (updatedShips, (ship, barrel)) => updatedShips.updated(ship.id, ship.copy(rums = 100.min(ship.rums + barrel.rums)))
    }

    val barrelsAfterShipMove = shipBarrelCollision.foldLeft(barrels) {
      case (remaining, (_, barrel)) => remaining - barrel.id
    }

    val shipMineCollision = for {
      ship <- ships.values
      mine <- CaribbeanContext.shipZone(ship).flatMap(cube => cubeToMine.get(cube))
      impacted = ships.values.filter(s => s == ship ||
        CaribbeanContext.cubeToNeighbors(mine.cube).intersect(CaribbeanContext.shipZone(s)).nonEmpty)
    } yield (mine, impacted)

    val shipsAfterShipMineCollision = shipMineCollision.foldLeft(shipsAfterShipBarrelCollision) {
      case (updatedShips, (mine, impacted)) =>
        impacted.foldLeft(updatedShips) {
          case (updatedShips2, ship) => {
            if (CaribbeanContext.shipZone(ship).contains(mine.cube)) {
              updatedShips2.updated(ship.id, ship.copy(rums = ship.rums - CaribbeanContext.highMineDamage))
            } else {
              updatedShips2.updated(ship.id, ship.copy(rums = ship.rums - CaribbeanContext.lowMineDamage))
            }
          }
        }
    }

    val minesAfterShipMove = shipMineCollision.foldLeft(mines) {
      case (remaining, (mine, _)) => remaining - mine.id
    }

    (shipsAfterShipMineCollision, barrelsAfterShipMove, minesAfterShipMove)
  }

  def moveShip(ship: Ship, speed: Int): Ship = {
    if (ship.speed >= speed) {
      val nextShip = ship.copy(position = ship.bow.toOffset)
      if (!CaribbeanContext.cubes.contains(nextShip.center)) {
        ship.copy(speed = 0)
      } else {
        nextShip
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
    CaribbeanContext.shipZone(one).intersect(CaribbeanContext.shipZone(other)).nonEmpty
  }


  override def judge(state: CaribbeanState): GameResult = ???
}
