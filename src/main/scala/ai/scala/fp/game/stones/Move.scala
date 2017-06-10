package ai.scala.fp.game.stones

import ai.scala.fp.geo.Pos

sealed trait Move

case class Slide(from: Pos, to: Pos) extends Move

case class Add(pos: Pos) extends Move
