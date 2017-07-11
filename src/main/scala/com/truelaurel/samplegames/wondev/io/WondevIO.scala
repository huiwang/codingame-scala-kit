package com.truelaurel.samplegames.wondev.io

import com.truelaurel.codingame.challenge.{ConsoleReader, ConsoleWriter, GameIO}
import com.truelaurel.math.geometry.{Direction, Pos}
import com.truelaurel.samplegames.wondev.domain._


object WondevIO extends GameIO[WondevState, WondevAction] {
  def readInitialState()(implicit reader: ConsoleReader): WondevState = {
    val size = reader.readInt()
    val unitsperplayer = reader.readInt()
    val context = WondevContext(size, unitsperplayer)
    read(context)
  }

  def readState(turn: Int, previous: WondevState)(implicit reader: ConsoleReader): WondevState =
    read(previous.context)(reader).copy(turn = turn)

  def writeAction(action: WondevAction)(implicit writer: ConsoleWriter): Unit =
    writer.println(action)

  private def read(context: WondevContext)(implicit reader: ConsoleReader): WondevState = {
    import context._

    val rows = Seq.fill(size)(reader.readLine())
    val myUnits = Seq.fill(unitsperplayer) {
      val Array(unitx, unity) = for (i <- reader.readLine() split " ") yield i.toInt
      Pos(unitx, unity)
    }
    val opUnits = Seq.fill(unitsperplayer) {
      val Array(unitx, unity) = for (i <- reader.readLine() split " ") yield i.toInt
      Pos(unitx, unity)
    }

    val legalactions = Seq.fill(reader.readInt()) {
      val Array(_type, _index, dir1, dir2) = reader.readLine() split " "
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
    WondevState(context, 0, heights, myUnits, opUnits, legalactions)
  }

}



