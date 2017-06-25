package com.truelaurel.samplegames.wondev.io

import com.truelaurel.codingame.challenge.{GameController, GamePlayer}
import com.truelaurel.math.geometry._
import com.truelaurel.samplegames.wondev.arena.WondevArena
import com.truelaurel.samplegames.wondev.domain._
import com.truelaurel.samplegames.wondev.strategy.WondevPlayer

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
    context.copy(previousHeightMap = WondevArena.next(state, actions).heightMap)
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

  override def warmup(player: GamePlayer[WondevState, WondevAction]): Unit = {
    WondevPlayer(true).reactTo(WondevState(WondevContext(5, 2), 18,
      Map(Pos(0, 2) -> 0, Pos(0, 0) -> 0, Pos(4, 0) -> 2, Pos(3, 4) -> 1, Pos(3, 1) -> 3, Pos(4, 1) -> 2, Pos(2, 0) -> 3, Pos(0, 3) -> 0, Pos(4, 4) -> 1, Pos(3, 0) -> 0, Pos(1, 1) -> 4, Pos(1, 4) -> 0, Pos(0, 4) -> 0, Pos(3, 2) -> 3, Pos(1, 3) -> 0, Pos(2, 2) -> 4, Pos(4, 2) -> 0, Pos(2, 4) -> 0, Pos(0, 1) -> 0, Pos(3, 3) -> 3, Pos(2, 3) -> 2, Pos(1, 2) -> 3, Pos(2, 1) -> 3, Pos(4, 3) -> 1, Pos(1, 0) -> 0), List(Pos(0, 2), Pos(2, 3)), List(Pos(3, 2), Pos(-1, -1)),
      List(LegalAction(Build, 0, N, N), LegalAction(Build, 0, N, NE), LegalAction(Build, 0, N, S), LegalAction(Build, 0, N, SE), LegalAction(Build, 0, S, E), LegalAction(Build, 0, S, N), LegalAction(Build, 0, S, NE), LegalAction(Build, 0, S, S), LegalAction(Build, 0, S, SE), LegalAction(Build, 0, SE, N), LegalAction(Build, 0, SE, NW), LegalAction(Build, 0, SE, S), LegalAction(Build, 0, SE, SE), LegalAction(Build, 0, SE, SW), LegalAction(Build, 0, SE, W), LegalAction(Build, 1, E, E), LegalAction(Build, 1, E, NE), LegalAction(Build, 1, E, S), LegalAction(Build, 1, E, SE), LegalAction(Build, 1, E, SW), LegalAction(Build, 1, E, W), LegalAction(Build, 1, NW, NE), LegalAction(Build, 1, NW, NW), LegalAction(Build, 1, NW, S), LegalAction(Build, 1, NW, SE), LegalAction(Build, 1, NW, SW), LegalAction(Build, 1, S, E), LegalAction(Build, 1, S, N), LegalAction(Build, 1, S, NE), LegalAction(Build, 1, S, NW), LegalAction(Build, 1, S, W), LegalAction(Build, 1, SE, E), LegalAction(Build, 1, SE, N), LegalAction(Build, 1, SE, NE), LegalAction(Build, 1, SE, NW), LegalAction(Build, 1, SE, W), LegalAction(Build, 1, SW, E), LegalAction(Build, 1, SW, N), LegalAction(Build, 1, SW, NE), LegalAction(Build, 1, SW, NW), LegalAction(Build, 1, SW, W), LegalAction(Build, 1, W, E), LegalAction(Build, 1, W, N), LegalAction(Build, 1, W, S), LegalAction(Build, 1, W, SE), LegalAction(Build, 1, W, SW), LegalAction(Build, 1, W, W), LegalAction(Push, 1, NE, E), LegalAction(Push, 1, NE, N), LegalAction(Push, 1, NE, NE))))
  }
}



