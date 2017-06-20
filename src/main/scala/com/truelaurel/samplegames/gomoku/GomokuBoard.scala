package com.truelaurel.samplegames.gomoku

import com.truelaurel.algorithm.game.GameState
import com.truelaurel.collection.{GridData, Pos}

import scala.annotation.tailrec

object GomokuBoard {
  def apply(size: Int): GomokuBoard =
    GomokuBoard(
      size,
      dataTrue = GridData(size),
      dataFalse = GridData(size),
      dataFree = GridData(size, rows = Array.fill(size)((1 << (size + 1)) - 1)))
}

case class GomokuBoard(
                        size: Int,
                        dataTrue: GridData,
                        dataFalse: GridData,
                        dataFree: GridData,
                        nextPlayer: Boolean = false)
  extends GameState[Boolean] {

  lazy val playedFalse: Set[Pos] = dataFalse.usedPos
  lazy val playedTrue: Set[Pos] = dataTrue.usedPos
  lazy val free: Set[Pos] = dataFree.usedPos

  def dataNext = if (nextPlayer) dataTrue else dataFalse

  def dataLast = if (nextPlayer) dataFalse else dataTrue

  def play(x: Int, y: Int): GomokuBoard = play(Pos(x, y))

  def play(p: Pos): GomokuBoard =
    if (nextPlayer)
      copy(
        nextPlayer = false,
        dataFree = dataFree - (p.x, p.y),
        dataTrue = dataTrue + (p.x, p.y))
    else
      copy(
        nextPlayer = true,
        dataFree = dataFree - (p.x, p.y),
        dataFalse = dataFalse + (p.x, p.y))

  def remove(p: Pos): GomokuBoard =
    copy(
      dataFree = dataFree + (p.x, p.y),
      dataTrue = dataTrue - (p.x, p.y),
      dataFalse = dataFalse - (p.x, p.y))

  def isFree(p: Pos): Boolean = dataFree.isUsed(p)

  override def toString: String = toText

  def toText: String = Seq.tabulate(size) { y =>
    Seq.tabulate(size) { x =>
      if (playedFalse(Pos(x, y))) 'F'
      else if (playedTrue(Pos(x, y))) 'T'
      else ' '
    }.mkString
  }.mkString("\n") + s"\n${nextPlayer.toString.toUpperCase.head} to play"

  def maxLength(player: Boolean) = {
    val played = if (player) playedTrue else playedFalse

    @tailrec
    def length(direction: (Int, Int), from: Pos, l: Int): Int = {
      val pos = Pos(from.x + direction._1, from.y + direction._2)
      if (played(pos)) length(direction, pos, l + 1)
      else l
    }

    if (played.isEmpty) 0 else {
      val lengths = for {
        d <- Pos.all
        p <- played
      } yield length(d, p, 1)
      lengths.max
    }
  }
}
