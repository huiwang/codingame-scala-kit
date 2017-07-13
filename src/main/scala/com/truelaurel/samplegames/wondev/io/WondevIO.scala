package com.truelaurel.samplegames.wondev.io

import com.truelaurel.codingame.challenge.GameIO
import com.truelaurel.math.geometry.{Direction, Pos}
import com.truelaurel.samplegames.wondev.domain._

import scala.io.StdIn._

object WondevIO extends GameIO[WondevContext, WondevState, WondevAction] {
  def readContext: WondevContext = {
    val size = readInt
    val unitsperplayer = readInt
    WondevContext(size, unitsperplayer)
  }

  def readState(turn: Int, context: WondevContext): WondevState =
    read(context)

  def read(context: WondevContext): WondevState = {
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

    val units = myUnits ++ opUnits

    val legalactions = Seq.fill(readInt) {
      val Array(_type, _index, dir1, dir2) = readLine split " "
      val index = _index.toInt
      val unit = units(index)
      val d1 = Direction(dir1)
      val d2 = Direction(dir2)
      val target1 = unit.neighborIn(d1)
      val target2 = target1.neighborIn(d2)
      if (_type == "MOVE&BUILD") {
        MoveBuild(index, target1, target2)
      } else {
        PushBuild(index, target1, target2)
      }
    }

    val heights = (for {
      (row: String, y: Int) <- rows.zipWithIndex
      (cell: Char, x) <- row.zipWithIndex
      h = if (cell == '.') -1 else cell.toInt
    } yield Pos(x, y) -> h).toMap
    WondevState(context, heights, units, legalactions)
  }

  override def writeAction(action: WondevAction): Unit = action match {
    case MoveBuild(unitIndex, move, build) => System.out.println(s"MOVE&BUILD $unitIndex $move $build")
    case PushBuild(unitIndex, build, push) => System.out.println(s"PUSH&BUILD $unitIndex $build $push")
  }
}



