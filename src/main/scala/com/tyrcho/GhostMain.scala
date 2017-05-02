package com.tyrcho

import scala.io.StdIn._

object Player extends App {
  var state = readInitialState()

  while (true) {
    state = update(state)
    val moves = state.nextMoves
    state.debugState()
    //    debug(state.factories(1))
    if (moves.isEmpty) println("WAIT")
    else println(moves.mkString(";"))
  }

  def update(state: GameState): GameState = {
    val entitycount = readInt
    val entities = Vector.fill(entitycount)(readEntity())
    val troops = entities.collect { case t: Troop => t }
    val factories = entities.collect { case f: Factory => f }
    val bombs = entities.collect { case b: Bomb => b }
    state.copy(troops = troops, factories = factories).updateBombs(bombs)
  }

  def readEntity(): Entity = {
    val Array(_entityid, entitytype, _arg1, _arg2, _arg3, _arg4, _arg5) = readLine split " "
    val id = _entityid.toInt
    val arg1 = _arg1.toInt
    val arg2 = _arg2.toInt
    val arg3 = _arg3.toInt
    entitytype match {
      case "FACTORY" => Factory(id,
        mine = if (arg1 == 1) Some(true) else if (arg1 == -1) Some(false) else None,
        cyborgs = arg2,
        maxProduction = arg3)
      case "TROOP" =>
        val arg4 = _arg4.toInt
        val arg5 = _arg5.toInt
        Troop(id,
          mine = arg1 == 1,
          from = arg2,
          to = arg3,
          cyborgs = arg4,
          timeLeft = arg5)
      case "BOMB" =>
        val arg4 = _arg4.toInt
        Bomb(id,
          mine = arg1 == 1,
          from = arg2,
          to = arg3,
          timeLeft = arg4)
    }
  }

  def readInitialState(): GameState = {
    val factorycount = readInt
    val linkcount = readInt
    GameState(Grid(Vector.fill(linkcount) {
      val Array(factory1, factory2, distance) = for (i <- readLine split " ") yield i.toInt
      Link(factory1, factory2, distance)
    }))
  }
}

