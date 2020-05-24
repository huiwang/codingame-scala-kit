package com.truelaurel.algorithm.graph

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by hwang on 02/03/2017.
  */
class ShortestPathTest extends FlatSpec with Matchers {

  behavior of "ShortestPathSpec"

  it should "testShortestItinearies" in {
    val itinearies = ShortestPath.shortestItinearies(2, Vector(Edge(0, 1, 2)))

    itinearies should be(
      Map(
        0 -> Map(
          1 -> Iti(2, Vector(0, 1)),
          0 -> Iti(0, Vector(0))
        ),
        1 -> Map(
          1 -> Iti(0, Vector(1))
        )
      )
    )
  }

  it should "shortest path should handle more elements" in {
    ShortestPath.shortestItinearies(
      5,
      Vector(
        Edge(2, 1, 4),
        Edge(2, 3, 3),
        Edge(1, 3, -2),
        Edge(4, 2, -1),
        Edge(3, 4, 2)
      )
    ) should be(
      Map(
        0 -> Map(0 -> Iti(0, Vector(0))),
        1 -> Map(
          2 -> Iti(-1, Vector(1, 3, 4, 2)),
          4 -> Iti(0, Vector(1, 3, 4)),
          1 -> Iti(0, Vector(1)),
          3 -> Iti(-2, Vector(1, 3))
        ),
        2 -> Map(
          2 -> Iti(0, Vector(2)),
          4 -> Iti(4, Vector(2, 1, 3, 4)),
          1 -> Iti(4, Vector(2, 1)),
          3 -> Iti(2, Vector(2, 1, 3))
        ),
        3 -> Map(
          2 -> Iti(1, Vector(3, 4, 2)),
          4 -> Iti(2, Vector(3, 4)),
          1 -> Iti(5, Vector(3, 4, 2, 1)),
          3 -> Iti(0, Vector(3))
        ),
        4 -> Map(
          2 -> Iti(-1, Vector(4, 2)),
          4 -> Iti(0, Vector(4)),
          1 -> Iti(3, Vector(4, 2, 1)),
          3 -> Iti(1, Vector(4, 2, 1, 3))
        )
      )
    )
  }

}
