package com.truelaurel.math.geometry.grid

case class BitGrid(gridData: GridData, masks: Masks) {
  def empty: Boolean = gridData.empty

  def complete: Boolean =
    masks.isComplete(gridData)

  def free = gridData.free

  def used = gridData.used

  def +(r: Int, c: Int) = {
    copy(gridData = gridData + (r, c))
  }

  def -(r: Int, c: Int) = {
    copy(gridData = gridData - (r, c))
  }

  def addCol(c: Int) = copy(gridData = gridData.addCol(c))

  def addCol(c: Int, minRow: Int, maxRow: Int) =
    copy(gridData = gridData.addCol(c, minRow, maxRow))

  def addRow(r: Int) = copy(gridData = gridData.addRow(r))

  def addRow(r: Int, minCol: Int, maxCol: Int) =
    copy(gridData = gridData.addRow(r, minCol, maxCol))

  def addDiag1(r: Int, c: Int, length: Int) =
    copy(gridData = gridData.addDiag1(r, c, length))

  def addDiag2(r: Int, c: Int, length: Int) =
    copy(gridData = gridData.addDiag2(r, c, length))
}

object BitGrid {

  def apply(size: Int, needed: Int): BitGrid =
    BitGrid(GridData(size), Masks(size, needed))
}
