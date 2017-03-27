package com.truelaurel.codingame.ghostcell

import com.truelaurel.codingame.graph.{Edge, ShortestPath}

case class GhostGraph(factoryCount: Int, undirectedEdges: Vector[Edge]) {
  val edges = undirectedEdges.flatMap(edge => Vector(edge, Edge(edge.to, edge.from, edge.distance)))
  val directDistances = edges.map(e => (e.from, e.to) -> e.distance).toMap
  val itineraries = ShortestPath.shortestItinearies(factoryCount, edges)
  val passThroughSegments: Iterable[Vector[Int]] = for {
    (from, toMap) <- itineraries
    (to, itinerary) <- toMap
    if from != to
    through = itinerary.path.tail.init
  } yield through
}
