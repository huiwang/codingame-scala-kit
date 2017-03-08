package codingame.scala.ghostcell

import codingame.scala.graph.{Edge, Itinerary, ShortestPath}

/**
  * Auto-generated code below aims at helping you parse
  * the standard input according to the problem statement.
  **/
object Player extends App {
  val factorycount = io.StdIn.readInt()
  // the number of factories
  val linkcount = io.StdIn.readInt() // the number of links between factories

  var links: Vector[Edge] = (for {
    i <- 0 until linkcount
    Array(factory1, factory2, distance) = for (i <- io.StdIn.readLine() split " ") yield i.toInt
    edge1 = Edge(factory1, factory2, distance)
    edge2 = Edge(factory2, factory1, distance)
  } yield Vector(edge1, edge2)).toVector.flatten

  private val itinearies: Map[Int, Map[Int, Itinerary]] = ShortestPath.shortestItinearies(factorycount, links)

  private val player = GhostCellPlayer

  // game loop
  while (true) {

    val entitycount = io.StdIn.readInt()
    // the number of entities (e.g. factories and troops)
    val entities = (for {
      i <- 0 until entitycount
      Array(entityid, entitytype, arg1, arg2, arg3, arg4, arg5) = io.StdIn.readLine().split(" ").map(_.toInt)
    } yield Entity(entityid, entitytype, arg1, arg2, arg3, arg4, arg5)).toVector

    val factories = entities.filter(_.entityType == "FACTORY").map(
      e => Factory(id = e.entityId, owner = e.arg1, cyborgs = e.arg2, production = e.arg3, again = e.arg4))

    val troops = entities.filter(_.entityType == "TROOP").map(
      e => Troop(id = e.entityId, owner = e.arg1, from = e.arg2, to = e.arg3, cyborgs = e.arg4, arrival = e.arg5))

    val bombs = entities.filter(_.entityType == "BOMB").map(
      e => Bomb(id = e.entityId, owner = e.arg1, from = e.arg2, to = e.arg3, explosion = e.arg4))

    val state = GhostCellGameState(itinearies, factories, troops, bombs)

    // Any valid action, such as "WAIT" or "MOVE source destination cyborgs"
    val actions = player.reactTo(state)
    if (actions.isEmpty) {
      println(WaitAction.command())
    } else {
      println(actions.map(a => a.command()).mkString(";"))
    }
  }
}