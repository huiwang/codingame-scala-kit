package com.truelaurel.algorithm.graph

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by hwang on 15/04/2017.
  */
class BreathFirstShortestPathTest extends FlatSpec with Matchers {

  behavior of "BreathFirstShortestPathTest"

  it should "findPaths with just one level" in {
    val graph = Map(
      1 -> Vector(2, 3)
    )
    val obstacles: Set[Int] = Set.empty
    val finder = new BreathFirstShortestPathFinder(graph, obstacles)
    finder.findPaths(1, Set(2, 3)) should be(
      Map(2 -> Vector(2), 3 -> Vector(3))
    )
  }

  it should "findPaths with multiple levels" in {
    val graph = Map(
      1 -> Vector(2, 3),
      3 -> Vector(5),
      2 -> Vector(4),
      4 -> Vector(6)
    )
    val obstacles: Set[Int] = Set.empty
    val finder = new BreathFirstShortestPathFinder(graph, obstacles)
    finder.findPaths(1, Set(5, 6)) should be(
      Map(5 -> Vector(3, 5), 6 -> Vector(2, 4, 6))
    )
  }

  it should "findPaths with  multiple levels and obstacles" in {
    val graph = Map(
      1 -> Vector(2, 3),
      3 -> Vector(5),
      2 -> Vector(4, 7),
      4 -> Vector(5, 6)
    )
    val obstacles = Set(3)
    val finder = new BreathFirstShortestPathFinder(graph, obstacles)
    finder.findPaths(1, Set(5, 6)) should be(
      Map(5 -> Vector(2, 4, 5), 6 -> Vector(2, 4, 6))
    )
  }

}
