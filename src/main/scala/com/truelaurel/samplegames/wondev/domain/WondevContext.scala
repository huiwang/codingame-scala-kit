package com.truelaurel.samplegames.wondev.domain

import com.truelaurel.math.geometry.{Direction, Pos}

object WondevContext {

  val neighborMapBySize: Map[Int, Map[Pos, Set[Pos]]] = Map(
    5 -> computeNeighborMap(5),
    6 -> computeNeighborMap(6),
    7 -> computeNeighborMap(7)
  )

  def computeNeighborMap(size: Int): Map[Pos, Set[Pos]] = (for {
    x <- 0 until size
    y <- 0 until size
    pos = Pos(x, y)
    neighbors = Direction.neighborsOf(pos, size)
  } yield pos -> neighbors).toMap
}

case class WondevContext(size: Int, unitsperplayer: Int, previousHeightMap: Map[Pos, Int] = Map.empty) {

  def neighborsMap: Map[Pos, Set[Pos]] = WondevContext.neighborMapBySize(size)

  val center = Pos(size / 2, size / 2)

  def distToCenter(p: Pos): Int = (p.x - center.x).abs + (p.y - center.y).abs
}