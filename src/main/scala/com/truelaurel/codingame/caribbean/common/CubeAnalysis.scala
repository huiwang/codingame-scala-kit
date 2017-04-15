package com.truelaurel.codingame.caribbean.common

import com.truelaurel.codingame.hexagons.{Cube, Offset}

/**
  * Created by hwang on 15/04/2017.
  */
object CubeAnalysis {
  val cubes: Vector[Cube] = (for {
    x <- 0 to 22
    y <- 0 to 20
  } yield Offset(x, y).toCube).toVector

  val cubeToNeighbors: Map[Cube, Vector[Cube]] = cubes
    .map(cube =>
      cube -> (0 to 5).map(cube.neighbor).filter(cubes.contains).toVector)
    .toMap

  def pathToBarrels(owner: Int, state: CaribbeanState): Map[Cube, Vector[Cube]] = {
    val ships = state.shipsOf(owner)
    ???
  }

  type BreathHistory = Map[Cube, Vector[Cube]]
  type Graph = Map[Cube, Vector[Cube]]

  def neighborWithHistory(from: Cube, history: Vector[Cube], visited: Set[Cube], g: Graph): BreathHistory = {
    g(from).filterNot(visited).map(neighbor => (neighbor, neighbor +: history)).toMap
  }

  def newNeighborsWithHistory(fringes: BreathHistory, visited: Set[Cube], g: Graph): BreathHistory = {
    fringes.flatMap({
      case (elem, history) => neighborWithHistory(elem, history, visited, g)
    })
  }

  def findPath(from: Cube, to: Cube, graph: Graph): Vector[Cube] = {
    path(to, Map(from -> Vector.empty), Set.empty, graph)
  }

  def path(to: Cube, fringes: BreathHistory, visited: Set[Cube], g: Graph): Vector[Cube] = {
    if (fringes.isDefinedAt(to)) fringes(to).reverse
    else {
      path(to, newNeighborsWithHistory(fringes, visited, g), fringes.keySet ++ visited, g)
    }
  }


}
