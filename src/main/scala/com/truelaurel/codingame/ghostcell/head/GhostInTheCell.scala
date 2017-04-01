package com.truelaurel.codingame.ghostcell.head

import com.truelaurel.codingame.engine.GameSimulator
import com.truelaurel.codingame.ghostcell.arena.GhostArena
import com.truelaurel.codingame.ghostcell.best.BestGhostCellPlayer
import com.truelaurel.codingame.ghostcell.common._
import com.truelaurel.codingame.graph.Edge

object Player extends App {
  val factoryCount = io.StdIn.readInt()
  val edgeCount = io.StdIn.readInt()

  var edges: Vector[Edge] = (for {
    i <- 0 until edgeCount
    Array(factory1, factory2, distance) = for (i <- io.StdIn.readLine() split " ") yield i.toInt
  } yield Edge(factory1, factory2, distance)).toVector

  private val ghostGraph = GhostGraph(factoryCount, edges)
  private val player = GhostCellPlayer(1)

  private var factoryProd: Map[Int, Int] = Map.empty

  private var turn = 1

  private var bombObserved: Map[Int, Int] = Map.empty

  private var bombBudget: Map[Int, Int] = Map(1 -> 2, -1 -> 2)

  var predicted: GhostCellGameState = null

  while (true) {

    val entityCount = io.StdIn.readInt()
    val entities = (for {
      i <- 0 until entityCount
      Array(entityid, entitytype, arg1, arg2, arg3, arg4, arg5) = io.StdIn.readLine().split(" ")
    } yield Entity(entityid.toInt, entitytype, arg1.toInt, arg2.toInt, arg3.toInt, arg4.toInt, arg5.toInt)).toVector

    val factories = entities.filter(_.entityType == "FACTORY").map(
      e => Fac(id = e.entityId, owner = e.arg1, cyborgs = e.arg2, production = factoryProd.getOrElse(e.entityId, 0).max(e.arg3), again = e.arg4))

    factoryProd = factories.map(fac => fac.id -> fac.production.max(factoryProd.getOrElse(fac.id, 0))).toMap


    val troops = entities.filter(_.entityType == "TROOP").map(
      e => Troop(id = e.entityId, owner = e.arg1, from = e.arg2, to = e.arg3, cyborgs = e.arg4, arrival = e.arg5))

    val bombs = entities.filter(_.entityType == "BOMB").map(
      e => Bomb(id = e.entityId, owner = e.arg1, from = e.arg2, to = e.arg3, explosion = e.arg4, observed = bombObserved.getOrElse(e.entityId, turn)))

    bombObserved = bombObserved ++ bombs.map(b => b.id -> b.observed)

    val state = GhostCellGameState(factories = factories, troops = troops, bombs = bombs, turn = turn, bombBudget = bombBudget, graph = ghostGraph)

    System.err.println(predicted)
    System.err.println(state)

    predicted = GameSimulator.simulate(1, state, GhostArena, Vector(GhostCellPlayer(1), BestGhostCellPlayer(-1)))

    val actions = player.reactTo(state)

    if (actions.isEmpty) {
      println(WaitAction.command())
    } else {
      println(actions.map(a => a.command()).mkString(";"))
    }

    bombBudget = BombBudget.computeBombBudget(actions, state, bombBudget)

    turn = turn + 1
  }

}