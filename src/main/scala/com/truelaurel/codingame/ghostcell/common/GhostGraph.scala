package com.truelaurel.codingame.ghostcell.common

import com.truelaurel.codingame.graph.{Edge, Iti, ShortestPath}

case class GhostGraph(factoryCount: Int, undirectedEdges: Vector[Edge]) {
  val edges: Vector[Edge] = undirectedEdges.flatMap(edge => Vector(edge, Edge(edge.to, edge.from, edge.distance)))
  val directDistances: Map[(Int, Int), Int] = edges.map(e => (e.from, e.to) -> e.distance).toMap
  val itineraries: Map[Int, Map[Int, Iti]] = ShortestPath.shortestItinearies(factoryCount, edges)
  val passThroughSegments: Iterable[Vector[Int]] = for {
    (from, toMap) <- itineraries
    (to, itinerary) <- toMap
    if from != to
    through = itinerary.path.tail.init
  } yield through
}
