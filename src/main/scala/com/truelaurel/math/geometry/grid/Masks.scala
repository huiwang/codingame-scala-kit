package com.truelaurel.math.geometry.grid

case class Masks(size: Int, needed: Int) {
  val empty = GridData(size)
  val matrixIndices = for {
    r0 <- 0 to size - needed
    c0 <- 0 to size - needed
  } yield (r0, c0)
  val matricesCompleted: Set[Long] =
    (preComputedMatricesRows ++ preComputedMatricesCols :+ preComputedMatricesDiag1 :+ preComputedMatricesDiag2).toSet

  def isComplete(grid: GridData) =
    matrixIndices.exists {
      case (r0, c0) =>
        val gsm = grid.subMatrix(r0, c0, needed)
        matricesCompleted.exists(m => (m & gsm) == m)
    }

  def preComputedMatricesRows: Seq[Long] =
    (0 until needed).map(row => (for {
      c <- 0 until needed
      i = row * needed + c
      bit = (1L << i)
    } yield bit).sum)

  def preComputedMatricesCols: Seq[Long] =
    (0 until needed).map(col => (for {
      r <- 0 until needed
      i = r * needed + col
      bit = (1L << i)
    } yield bit).sum)

  def preComputedMatricesDiag1: Long =
    (for {
      r <- 0 until needed
      c = r
      i = r * needed + c
      bit = (1L << i)
    } yield bit).sum

  def preComputedMatricesDiag2: Long =
    (for {
      r <- 0 until needed
      c = needed - 1 - r
      i = r * needed + c
      bit = (1L << i)
    } yield bit).sum

}
