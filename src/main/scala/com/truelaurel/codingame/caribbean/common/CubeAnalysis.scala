package com.truelaurel.codingame.caribbean.common

import com.truelaurel.codingame.graph.BreathFirstShortestPathFinder
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
    val finder = new BreathFirstShortestPathFinder(cubeToNeighbors, state.mines.map(_.cube).toSet)
    ships.flatMap(ship => finder.findPaths(ship.cube, state.barrels.map(_.cube).toSet)).toMap
  }


}
