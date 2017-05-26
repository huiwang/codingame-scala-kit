
package com.tyrcho.ghostinthecell

sealed trait Move

case class TroopMove(from: Int, to: Int, cyborgs: Int, target: Option[Int] = None) extends Move {
  def isStayMove: Boolean = from == to

  override def toString: String = if (isStayMove)
      s"MSG $cyborgs stay in $from"
    else
      s"MOVE $from $to $cyborgs"
}

case class IncMove(factory: Int) extends Move {
  override def toString: String = s"INC $factory"
}

case class BombMove(from: Int, to: Int) extends Move {
  override def toString: String = s"BOMB $from $to"
}
