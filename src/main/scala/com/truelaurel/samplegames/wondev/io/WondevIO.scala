package com.truelaurel.samplegames.wondev.io

import com.truelaurel.math.geometry.{Direction, Pos}
import com.truelaurel.samplegames.wondev.domain._

import scala.io.StdIn._

object WondevIO {
  def readInitialState(): WondevState = {
    val size = readInt
    val unitsperplayer = readInt
    WondevState(WondevContext(size, unitsperplayer))
  }

  def readState(turn: Int, previous: WondevState): WondevState =
    read(previous).copy(turn = turn)

  def read(previous: WondevState): WondevState = {
    import previous.context._

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
      val Array(_type, _index, dir1, dir2) = readLine split " "
      val index = _index.toInt
      if (_type == "MOVE&BUILD") {
        MoveBuild(index, Direction(dir1), Direction(dir2))
      } else {
        MovePush(index, Direction(dir1), Direction(dir2))
      }
    }

    val heights = (for {
      (row: String, y: Int) <- rows.zipWithIndex
      (cell: Char, x) <- row.zipWithIndex
      h = if (cell == '.') -1 else cell.toInt
    } yield Pos(x, y) -> h).toMap
    WondevState(previous.context, 0, heights, myUnits, opUnits, legalactions)
  }

  def writeAction(action: WondevAction): Unit = ???
}



