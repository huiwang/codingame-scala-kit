package com.truelaurel.math.geometry.grid

import com.truelaurel.math.geometry.Pos
import org.scalatest.{FlatSpec, Matchers}

class GridDataSpec extends FlatSpec with Matchers {

  "a GridData" should "list free neighbours (vert/horiz)" in {
    // AA.
    // .X.
    // ...
    val grid = GridData(3) + (0, 0) + (1, 0)
    grid.freeNeighbours4(Pos(1, 1)) should contain theSameElementsAs Seq(Pos(0, 1), Pos(2, 1), Pos(1, 2))
  }

}