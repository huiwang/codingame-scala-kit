package ai.scala.fp.geo

import scala.collection.BitSet

case class GridData(size: Int, rows: Array[Long]) {

  def empty: Boolean = rows.forall(0.==)

  def neighbours4(p: Pos): Seq[Pos] =
    p.neighbours4.filter(isValidPos)

  def freeNeighbours4(p: Pos): Seq[Pos] =
    p.neighbours4.filter(pos => isValidPos(pos) && isFree(pos))

  def isValidPos(p: Pos): Boolean = p.x >= 0 &&
    p.y >= 0 &&
    p.x < size &&
    p.y < size

  def +(r: Int, c: Int): GridData = {
    copy(rows = rows.updated(r, rows(r) | 1 << c))
  }

  def -(r: Int, c: Int): GridData = {
    copy(rows = rows.updated(r, rows(r) & ~(1 << c)))
  }

  lazy val free: Iterable[(Int, Int)] =
    for {
      r <- 0 until size
      row = rows(r)
      c <- 0 until size
      if (row & (1 << c)) == 0
    } yield (r, c)

  def isFree(p: Pos): Boolean =
    isFree(p.x, p.y)

  def isFree(r: Int, c: Int): Boolean =
    (rows(r) & (1 << c)) == 0

  def isUsed(p: Pos): Boolean =
    isUsed(p.x, p.y)

  def isUsed(r: Int, c: Int): Boolean =
    (rows(r) & (1 << c)) != 0

  lazy val used: Iterable[(Int, Int)] =
    for {
      r <- 0 until size
      row = rows(r)
      c <- 0 until size
      if (row & (1 << c)) != 0
    } yield (r, c)

  lazy val usedPos: Set[Pos] =
    used.map { case (r, c) => Pos(r, c) }.toSet

  def addCol(c: Int, minRow: Int = 0, maxRow: Int = size) =
    (minRow until maxRow).foldLeft(this) {
      case (bg, r) => bg + (r, c)
    }

  def addRow(r: Int, minCol: Int = 0, maxCol: Int = size) =
    (minCol until maxCol).foldLeft(this) {
      case (bg, c) => bg + (r, c)
    }

  def addDiag1(r: Int, c: Int, size: Int = size) =
    (0 until size).foldLeft(this) {
      case (bg, i) => bg + (r + i, c + i)
    }

  def addDiag2(r: Int, c: Int, size: Int = size) =
    (0 until size).foldLeft(this) {
      case (bg, i) => bg + (r + i, c - i)
    }

  def subMatrix(r0: Int, c0: Int, subSize: Int): Long = {
    var sum = 0L
    var x = 0
    while (x < subSize) {
      val r = x + r0
      var y = 0
      while (y < subSize) {
        val c = y + c0
        val i = x * subSize + y
        val bit = (1L << i) & (rows(r) >> c << i)
        sum += bit
        y += 1
      }
      x += 1
    }
    sum
  }
}

object GridData {
  def apply(size: Int): GridData =
    GridData(size, rows = Array.fill(size)(0L))

  val fullGridCache = collection.mutable.Map.empty[Int, BitSet]

  def fullGrid(size: Int) = fullGridCache.getOrElseUpdate(size, {
    val all = for {
      r <- 0 until size
      c <- 0 until size
    } yield r * size + c
    BitSet(all: _*)
  })

  def full(size: Int) = GridData(size, Array.fill(size)((1L << size + 1) - 1))
}