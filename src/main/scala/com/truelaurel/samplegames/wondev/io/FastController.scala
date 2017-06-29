package com.truelaurel.samplegames.wondev.io

import com.truelaurel.codingame.challenge.GameController
import com.truelaurel.codingame.logging.CGLogger
import com.truelaurel.math.geometry._
import com.truelaurel.math.geometry.grid.FastGrid
import com.truelaurel.samplegames.wondev.domain._
import com.truelaurel.samplegames.wondev.io.FastController._

import scala.io.StdIn._

case class FastController(size: Int) extends GameController[FastContext, FastState, WondevAction] {
  val grid = FastGrid(size)

  def readContext: FastContext = {
    val unitsperplayer = readInt
    FastContext(size, unitsperplayer)
  }

  def readState(turn: Int, context: FastContext): FastState =
    read(context).copy(turn = turn)

  def nextContext(context: FastContext, state: FastState, actions: Vector[WondevAction]): FastContext = {
    val next = state.applyAction(actions.head)
    context.copy(stateAfterMyAction = Some(next))
  }

  def read(context: FastContext): FastState = {
    val heights = readHeights(context)
    val mine = readUnits(context.unitsperplayer).toArray.map(grid.pos)
    val guess = context.guessOppPos(heights, mine).getOrElse(FastContext.notSeenByMine(mine, grid, heights))
    CGLogger.info(s"guessed opponent: ${guess.toSeq.map(grid.pos)}")
    val op = readUnits(context.unitsperplayer).toArray.map(p => if (p == Pos(-1, -1)) guess.head else grid.pos(p))
    val state = FastState(size, mine, op).copy(heights = heights)
    readActions
    state
  }


  private def readUnits(unitsperplayer: Int) = {
    Seq.fill(unitsperplayer) {
      val Array(unitx, unity) = for (i <- readLine split " ") yield i.toInt
      Pos(unitx, unity)
    }
  }

  private def readHeights(context: FastContext) = {
    val rows = Seq.fill(context.size)(readLine)
    parseHeights(rows, grid)
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

object FastController {
  def parseHeights(rows: Seq[String], grid: FastGrid): Array[Int] = {
    val heights = for {
      (row: String, y: Int) <- rows.zipWithIndex
      (cell: Char, x) <- row.zipWithIndex
      h = if (cell == '.') -1 else cell - '0'
    } yield grid.pos(Pos(x, y)) -> h
    heights.sortBy(_._1).map(_._2)
  }.toArray
}
