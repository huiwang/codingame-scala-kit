package com.truelaurel.samplegames.wondev

import com.truelaurel.math.geometry.Pos

import scala.io.StdIn.{readInt, readLine}

case class WondevState(turn: Int,
                       rows: Seq[String],
                       myUnits: Seq[Pos],
                       opUnits: Seq[Pos],
                       legalActions: Seq[LegalAction])

object WondevState {
  def read(context: WondevContext) = {
    import context._

    val rows = Seq.fill(size)(readLine)
    val myUnits = Seq.fill(unitsperplayer) {
      val Array(unitx, unity) = for (i <- readLine split " ") yield i.toInt
      Pos(unitx, unity)
    }
    val opUnits = Seq.fill(unitsperplayer) {
      val Array(unitx, unity) = for (i <- readLine split " ") yield i.toInt
      Pos(unitx, unity)
    }

    val legalactions = Seq.fill(readInt) {
      val Array(t, _index, dir1, dir2) = readLine split " "
      val index = _index.toInt
      LegalAction(t, index, dir1, dir2)
    }
    WondevState(0, rows, myUnits, opUnits, legalactions)
  }
}

