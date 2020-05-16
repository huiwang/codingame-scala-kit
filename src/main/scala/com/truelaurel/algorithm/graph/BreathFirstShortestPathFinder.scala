package com.truelaurel.algorithm.graph

class BreathFirstShortestPathFinder[T](graph: Map[T, Vector[T]], obstacles: Set[T]) {
  //elements at flood front and path toward it from search source
  type Paths = Map[T, Vector[T]]

  def findPath(source: T, target: T): Vector[T] = {
    findPaths(source, Set(target)).getOrElse(source, Vector.empty)
  }

  def findPaths(source: T, targets: Set[T]): Paths = {
    path(targets, Map(source -> Vector.empty), Set.empty, Map.empty).mapValues(_.reverse).toMap
  }

  private def path(targets: Set[T], paths: Paths, visited: Set[T], found: Paths): Paths = {
    if (targets.isEmpty) {
      found
    } else {
      val reached = paths.keySet
      if (reached.subsetOf(visited)) {
        //nothing more to explore
        found
      } else {
        val reachedTargets = reached.intersect(targets)
        path(targets -- reachedTargets,
          newNeighborsWithHistory(paths, visited),
          reached ++ visited,
          found ++ paths.filterKeys(reachedTargets))
      }
    }
  }

  private def newNeighborsWithHistory(fringes: Paths, visited: Set[T]): Paths = {
    fringes.flatMap({
      case (elem, history) =>
        graph.getOrElse(elem, Vector.empty)
          .filterNot(visited)
          .filterNot(obstacles)
          .map(neighbor => (neighbor, neighbor +: history))
          .toMap
    })
  }
}
