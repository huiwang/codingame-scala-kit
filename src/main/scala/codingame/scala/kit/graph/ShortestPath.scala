package codingame.scala.kit.graph

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


case class Edge(from: Int, to: Int, distance: Int)

case class Itinerary(distance: Int, path: Vector[Int])
/**
  * Floyd–Warshall algorithm
  */
object ShortestPath {
  def shortestItinearies(vertexCount: Int, edges: Vector[Edge]): Map[Int, Map[Int, Itinerary]] = {
    val n = vertexCount
    val inf = Int.MaxValue

    // Initialize distance matrix.
    val ds = Array.fill[Int](n, n)(inf)
    for (i <- 0 until n) ds(i)(i) = 0
    edges.foreach(e => ds(e.from)(e.to) = e.distance)
    // Initialize next vertex matrix.
    val ns = Array.fill[Int](n, n)(-1)

    // Here goes the magic!
    for (k <- 0 until n; i <- 0 until n; j <- 0 until n)
      if (ds(i)(k) != inf && ds(k)(j) != inf && ds(i)(k) + ds(k)(j) < ds(i)(j)) {
        ds(i)(j) = ds(i)(k) + ds(k)(j)
        ns(i)(j) = k
      }

    // Helper function to carve out paths from the next vertex matrix.
    def extractPath(path: ArrayBuffer[Int], i: Int, j: Int) {
      if (ds(i)(j) == inf) return
      val k = ns(i)(j)
      if (k != -1) {
        extractPath(path, i, k)
        path.append(k)
        extractPath(path, k, j)
      }
    }

    // Extract paths.
    val itenearies = mutable.Map[Int, Map[Int, Seq[Int]]]()
    for (i <- 0 until n) {
      val ps = mutable.Map[Int, Seq[Int]]()
      for (j <- 0 until n)
        if (ds(i)(j) != inf) {
          val p = new ArrayBuffer[Int]()
          p.append(i)
          if (i != j) {
            extractPath(p, i, j)
            p.append(j)
          }
          ps(j) = p
        }
      itenearies(i) = ps.toMap
    }

    // Return extracted paths.
    itenearies.map {
      case (from, dests) => {
        from -> dests.map {
          case (dest, path) => {
            val (totalDist, _) = path.foldLeft(0, from) {
              case ((total, pre), cur) =>
                (total + ds(pre)(cur), cur)
            }
            dest -> Itinerary(totalDist, path.toVector)
          }
        }
      }
    }.toMap
  }
}
