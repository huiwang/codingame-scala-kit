package com.truelaurel.samplegames.wondev.domain

import com.truelaurel.math.geometry.Pos

import scala.collection.mutable.ArrayBuffer


class FastWondevState(val size: Int,
                      private val units: Array[Pos],
                      private val height: Array[Array[Int]],
                      var nextPlayer: Boolean) {
  private val neighborTable = WondevContext.neighborMapBySize(size)

  private val freeCellTable: Array[Array[Boolean]] = extractFreeCellTable

  def heightOf(p: Pos): Int = height(p.x)(p.y)

  def neighborOf(pos: Pos): Array[Pos] = {
    neighborTable(pos.x)(pos.y)
  }

  def isFree(pos: Pos): Boolean = freeCellTable(pos.x)(pos.y)

  def unitAt(id: Int): Pos = units(id)

  def unitIdOf(pos: Pos): Int = units.indexOf(pos)

  def feasibleOppo(): Set[Pos] = {
    val feasible: ArrayBuffer[Pos] = ArrayBuffer.empty
    var i = 0
    while (i < size) {
      var j = 0
      while (j < size) {
        val pos = Pos(i, j)
        if (WondevContext.isPlayable(heightOf(pos)) && units(0) != pos && units(1) != pos) {
          feasible.append(pos)
        }
        j += 1
      }
      i += 1
    }
    feasible.toSet
  }


  def myUnits: Array[Pos] = units.take(2)

  def opUnits: Array[Pos] = units.takeRight(2)

  def hasSameHeightMap(that: FastWondevState): Boolean = {
    var i = 0
    while (i < height.length) {
      val row = height(i)
      var j = 0
      while (j < row.length) {
        val h = row(j)
        if (that.height(i)(j) != h) {
          return false
        }
        j += 1
      }
      i += 1
    }
    true
  }


  def moveUnit(id: Int, to: Pos): () => Unit = {
    val from = units(id)
    units(id) = to
    freeCellTable(from.x)(from.y) = true
    freeCellTable(to.x)(to.y) = false
    () => {
      moveUnit(id, from)
    }
  }

  def increaseHeight(pos: Pos): () => Unit = {
    height(pos.x)(pos.y) += 1
    () => {
      height(pos.x)(pos.y) -= 1
    }
  }

  def swapPlayer(): () => Unit = {
    nextPlayer = !nextPlayer
    () => {
      swapPlayer()
    }
  }

  def setOppo(oppo : Set[Pos]): Unit = {
    val oppoSeq = oppo.toSeq
    units(3) = oppoSeq(1)
    units(4) = oppoSeq(2)
  }

  private def extractFreeCellTable = {
    val occupyTable: Array[Array[Boolean]] = Array.fill(size, size)(true)
    units.foreach(u => if (u.x != -1) {
      occupyTable(u.x)(u.y) = false
    })
    occupyTable
  }


  def canEqual(other: Any): Boolean = other.isInstanceOf[FastWondevState]

  override def equals(other: Any): Boolean = other match {
    case that: FastWondevState =>
      (that canEqual this) &&
        size == that.size &&
        (units sameElements that.units) &&
        height.deep == that.height.deep &&
        nextPlayer == that.nextPlayer
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(size, units, height, nextPlayer)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }

  def copy(): FastWondevState = {
    new FastWondevState(size, units.clone(), height.clone(), nextPlayer)
  }

  override def toString: String = s"FastWondevState(size=$size, units=${units.mkString("[", ",", "]")}, " +
    s"height=${height.map(_.mkString("[", ",", "]")).mkString("[", ",", "]")}, nextPlayer=$nextPlayer)"
}

object FastWondevState {
  def fromSlowState(wondevState: WondevState): FastWondevState = {
    val height: Array[Array[Int]] = Array.fill(wondevState.size)(Array.fill(wondevState.size)(0))
    for {
      (pos, h) <- wondevState.heightMap
    } {
      height(pos.x)(pos.y) = h
    }
    new FastWondevState(wondevState.size, wondevState.units.toArray, height, wondevState.nextPlayer)
  }
}