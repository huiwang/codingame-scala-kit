package com.truelaurel.codingame.minimax

/**
  * Representation of a problem that can be solved with Minimax algorithm
  *
  * @tparam Node structure representing the game state
  * @tparam Edge a legal action that can be applied on a node
  */
trait MinimaxRepresentation[Node, Edge] {

  /**
    * Gets the starting node for the maximizing player
    */
  def root: Node

  /**
    * Returns edges leading the given node to its children nodes. If the given node is a leaf node, empty is returned
    */
  def edgesOf(node: Node): Iterable[Edge]

  /**
    * Returns the child node by applying the edge to the given state.
    */
  def childOf(node: Node, edge: Edge): Node

  /**
    * Assess the given node. A Greater score means a better situation for the maximizing player
    */
  def assess(node: Node): Double
}

