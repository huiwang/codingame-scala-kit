package com.truelaurel.samplegames.wondev.domain

import java.util

import com.truelaurel.math.geometry.Pos


class FastWondevState(private val size: Int,
                      private val units: Array[Pos],
                      private val height: Array[Array[Int]],
                      private var nextPlayer: Boolean) {

  private val neighborTable = WondevContext.neighborMapBySize(size)

  private val freeCellTable: Array[Array[Boolean]] = extractFreeCellTable

  def heightOf(p: Pos): Int = height(p.x)(p.y)

  def neighborOf(pos: Pos): Array[Pos] = {
    neighborTable(pos.x)(pos.y)
  }

  def isFree(pos: Pos): Boolean = freeCellTable(pos.x)(pos.y)

  def unitAt(id: Int): Pos = units(id)

  def unitIdOf(pos: Pos) : Int = units.indexOf(pos)

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
    val height: Array[Int] = Array.ofDim(wondevState.size * wondevState.size)
    for {
      (pos, h) <- wondevState.heightMap
    } {
      height(pos.x + pos.y * wondevState.size) = h
    }
    new FastWondevState(wondevState.size, wondevState.units.toArray, null, wondevState.nextPlayer)
  }
}