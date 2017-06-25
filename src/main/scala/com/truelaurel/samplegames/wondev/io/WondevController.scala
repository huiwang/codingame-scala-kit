package com.truelaurel.samplegames.wondev.io

import com.truelaurel.codingame.challenge.GameController
import com.truelaurel.math.geometry.{Direction, Pos}
import com.truelaurel.samplegames.wondev.domain._

import scala.io.StdIn._

object WondevController extends GameController[WondevContext, WondevState, WondevAction] {
  def readContext: WondevContext = {
    val size = readInt
    val unitsperplayer = readInt
    WondevContext(size, unitsperplayer)
  }

  def readState(turn: Int, context: WondevContext): WondevState =
    read(context).copy(turn = turn)

  def nextContext(context: WondevContext, state: WondevState, actions: Vector[WondevAction]): WondevContext = {
    context
  }

  def read(context: WondevContext): WondevState =
    WondevState(
      context,
      0,
      readHeights(context),
      readUnits(context.unitsperplayer),
      readUnits(context.unitsperplayer),
      readActions)

  private def readUnits(unitsperplayer: Int) = {
    Seq.fill(unitsperplayer) {
      val Array(unitx, unity) = for (i <- readLine split " ") yield i.toInt
      Pos(unitx, unity)
    }
  }

  private def readHeights(context: WondevContext) = {
    val rows = Seq.fill(context.size)(readLine)
    (for {
      (row: String, y: Int) <- rows.zipWithIndex
      (cell: Char, x) <- row.zipWithIndex
      h = if (cell == '.') -1 else cell - '0'
    } yield Pos(x, y) -> h).toMap
  }

  private def readActions = {
    Seq.fill(readInt) {
      val Array(_type, _index, dir1, dir2) = readLine split " "
      val index = _index.toInt
      if (_type == "MOVE&BUILD") {
        LegalAction(Build, index, Direction(dir1), Direction(dir2))
      } else {
        LegalAction(Push, index, Direction(dir1), Direction(dir2))
      }
    }
  }
}



