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

  var links: Vector[Edge] = Vector.empty

  for (i <- 0 until linkcount) {
    val Array(factory1, factory2, distance) = for (i <- io.StdIn.readLine() split " ") yield i.toInt
    links = links :+ Edge(factory1, factory2, distance) :+ Edge(factory2, factory1, distance)
  }

  var factories: Vector[Factory] = Vector.empty
  var troops: Vector[Troop] = Vector.empty
  var bombs: Vector[Bomb] = Vector.empty

  private val itinearies: Map[Int, Map[Int, Itinerary]] = ShortestPath.shortestItinearies(factorycount, links)


  private val player = GhostCellPlayer

  // game loop
  while (true) {
    factories = Vector.empty
    troops = Vector.empty
    bombs = Vector.empty

    val entitycount = io.StdIn.readInt() // the number of entities (e.g. factories and troops)
    for (i <- 0 until entitycount) {
      val Array(_entityid, entitytype, _arg1, _arg2, _arg3, _arg4, _arg5) = io.StdIn.readLine() split " "
      val entityid = _entityid.toInt
      val arg1 = _arg1.toInt
      val arg2 = _arg2.toInt
      val arg3 = _arg3.toInt
      val arg4 = _arg4.toInt
      val arg5 = _arg5.toInt

      entitytype match {
        case "FACTORY" => {
          factories = factories :+ Factory(entityid, owner = arg1, cyborgs = arg2, production = arg3, again = arg4)
        }
        case "TROOP" => {
          troops = troops :+ Troop(entityid, owner = arg1, from = arg2, to = arg3, cyborgs = arg4, arrival = arg5)
        }

        case "BOMB" => {
          bombs = bombs :+ Bomb(entityid, owner = arg1, from = arg2, to = arg3, explosion = arg4)
        }


        case _ => throw new IllegalArgumentException("Unsupported type " + entitytype)
      }

    }

    val state = GhostCellGameState(itinearies, factories, troops, bombs)

    System.err.println("fac " + factories)
    // Write an action using println
    // To debug: Console.err.println("Debug messages...")

    // Any valid action, such as "WAIT" or "MOVE source destination cyborgs"
    val actions = player.reactTo(state)
    if (actions.isEmpty) {
      println(WaitAction.command())
    } else {
      println(actions.map(a => a.command()).mkString(";"))
    }
  }
}