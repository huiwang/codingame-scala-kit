package com.truelaurel.samplegames.wondev.domain

import com.truelaurel.algorithm.game.GameState
import com.truelaurel.math.geometry.Pos

import scala.collection.mutable.ArrayBuffer


class FastWondevState(val size: Int,
                      private val units: Array[Pos],
                      private val height: Array[Array[Int]],
                      var nextPlayer: Boolean) extends GameState[Boolean]{

  private val neighborTable = WondevContext.neighborMapBySize(size)

  private val freeCellTable: Array[Array[Boolean]] = extractFreeCellTable

  val writable = new Writable()

  val readable = new Readable()

  class Writable {

    type Operation = () => Unit

    private var stack: List[List[Operation]] = Nil
    private var current: List[Operation] = Nil

    def start(): Unit = {
      current = Nil
    }

    def setOppo(oppo: Set[Pos]): FastWondevState = {
      val oldUnit2 = units(2)
      val oldUnit3 = units(3)
      val oppoSeq = oppo.toSeq
      val newUnit2 = oppoSeq.head
      val newUnit3 = oppoSeq(1)
      units(2) = newUnit2
      units(3) = newUnit3
      val oldFree2 = readable.isFree(units(2))
      val oldFree3 = readable.isFree(units(3))
      setOccupied(freeCellTable, units(2))
      setOccupied(freeCellTable, units(3))
      push(() => {
        units(2) = oldUnit2
        units(3) = oldUnit3
        freeCellTable(newUnit2.x)(newUnit2.y) = oldFree2
        freeCellTable(newUnit3.x)(newUnit3.y) = oldFree3
      })
    }

    def moveUnit(id: Int, to: Pos): FastWondevState = {
      val from: Pos = doMove(id, to)
      push(() => {
        doMove(id, from)
      })
    }

    def increaseHeight(pos: Pos): FastWondevState = {
      height(pos.x)(pos.y) += 1
      push(() => {
        height(pos.x)(pos.y) -= 1
      })
    }

    def swapPlayer(): FastWondevState = {
      doSwap()
      push(() => {
        doSwap()
      })
    }

    private def doSwap() = {
      nextPlayer = !nextPlayer
    }

    def end(): Unit = {
      stack = current :: stack
    }

    def undo(): Unit = {
      stack.head.foreach(operation => operation.apply())
      stack = stack.tail
    }

    private def push(operation: Operation): FastWondevState = {
      current = operation :: current
      FastWondevState.this
    }
  }

  private def doMove(id: Int, to: Pos) = {
    val from = units(id)
    units(id) = to
    freeCellTable(from.x)(from.y) = true
    freeCellTable(to.x)(to.y) = false
    from
  }

  class Readable {
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

    def findIncreasedCell : Option[Pos] = {
      var i = 0
      while (i < size) {
        var j = 0
        while (j < size) {
          val pos = Pos(i, j)
          if(heightOf(pos) == 1) {
            return Some(pos)
          }
          j += 1
        }
        i += 1
      }
      None
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
  }


  private def extractFreeCellTable = {
    val occupyTable: Array[Array[Boolean]] = Array.fill(size, size)(true)
    units.foreach(u => setOccupied(occupyTable, u))
    occupyTable
  }


  private def setOccupied(occupyTable: Array[Array[Boolean]], u: Pos) = {
    if (u.x != -1) {
      occupyTable(u.x)(u.y) = false
    }
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