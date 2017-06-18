package com.truelaurel.codingame.minimax

/**
  * Created by hwang on 18/06/2017.
  */
class MinimaxAlgorithm[Node, Edge] {

  type Repr = MinimaxRepresentation[Node, Edge]

  def search(repr: Repr, depth: Int): Edge = {
    val root = repr.root
    val assessedEdges = for {
      edge <- repr.edgesOf(root)
      child = repr.childOf(root, edge)
    } yield (edge, minimax(repr, child, depth - 1, maximizingPlayer = false))
    assessedEdges.maxBy(_._2)._1
  }

  private def minimax(repr: Repr, node: Node, depth: Int, maximizingPlayer: Boolean): Double = {
    val edges = repr.edgesOf(node)
    if (depth == 0 || edges.isEmpty) {
      repr.assess(node)
    } else {
      val children = edges.map(e => minimax(repr, repr.childOf(node, e), depth - 1, !maximizingPlayer))
      if (maximizingPlayer) {
        children.max
      } else {
        children.min
      }
    }
  }
}
