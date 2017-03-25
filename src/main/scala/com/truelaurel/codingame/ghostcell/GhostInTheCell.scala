package com.truelaurel.codingame.ghostcell

import com.truelaurel.codingame.graph.{Edge, Iti}

object Player extends App {
  val factoryCount = io.StdIn.readInt()
  val edgeCount = io.StdIn.readInt()

  var edges: Vector[Edge] = (for {
    i <- 0 until edgeCount
    Array(factory1, factory2, distance) = for (i <- io.StdIn.readLine() split " ") yield i.toInt
  } yield Edge(factory1, factory2, distance)).toVector

  private val directDist = (edges.map(e => (e.from, e.to) -> e.distance) ++ edges.map(e => (e.to, e.from) -> e.distance)).toMap

  private val player = GhostCellPlayer

  private var factoryProd : Map[Int, Int] = Map.empty

  private var turn = 1

  private var bombBirth : Map[Int, Int] = Map.empty

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
      e => Bomb(id = e.entityId, owner = e.arg1, from = e.arg2, to = e.arg3, explosion = e.arg4, birth = bombBirth.getOrElse(e.entityId, turn)))

    bombBirth = bombBirth ++ bombs.map(b => b.id -> b.birth)

    val state = GhostCellGameState(factories, troops, bombs, turn, edges)


    System.err.println(state)

    val actions = player.reactTo(state)

    if (actions.isEmpty) {
      println(WaitAction.command())
    } else {
      println(actions.map(a => a.command()).mkString(";"))
    }

    turn = turn + 1

  }
}