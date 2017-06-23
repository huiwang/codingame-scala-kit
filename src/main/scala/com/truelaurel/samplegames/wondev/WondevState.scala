package com.truelaurel.samplegames.wondev

import com.truelaurel.collection.IterableUtil._
import com.truelaurel.math.geometry.Pos

import scala.io.StdIn.{readInt, readLine}

case class WondevState(turn: Int,
                       rows: Seq[String],
                       myUnits: Seq[Pos] = Nil,
                       opUnits: Seq[Pos] = Nil,
                       legalActions: Seq[LegalAction] = Nil) {

  def apply(action: LegalAction): WondevState = {
    val myPos = myUnits.head + action.dir1
    val tgt = myPos + action.dir2

    copy(myUnits = Seq(myPos)).up(tgt)
  }

  def up(pos: Pos) = copy(
    rows = rows.updatef(pos.y, _.updatef(pos.x, c => (c + 1).toChar))
  )

  def height(pos: Pos): Option[Int] = rows(pos.y)(pos.x) match {
    case '.' => None
    case i => Some(i - '0')
  }
}

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

